(ns plotly.agent
  (:require [clj-http.client           :as client]
            [cheshire.core             :as json]
            [clojure.java.io           :as io]
            [clojure.string            :as str]
            [compojure.core            :refer [defroutes GET POST]]
            [compojure.route           :as route]
            [hiccup.page               :refer [html5]]
            [ring.adapter.jetty        :refer [run-jetty]]
            [ring.middleware.params    :refer [wrap-params]]
            [clojure.pprint            :refer [pprint]])
  (:import (org.everit.json.schema.loader SchemaLoader)
           (org.everit.json.schema Schema)
           (org.json JSONObject JSONTokener)))

;; -----------------------------------------------------------------------------
;; API Key and Endpoint
;; -----------------------------------------------------------------------------

(def api-key
  "Retrieve the API key from the environment or use a placeholder."
  (or (System/getenv "OPENAI_API_KEY")
      "your-api-key-here"))
(comment
  ;; Example: Print the API key.
  (println api-key))

(def openai-api-url
  "The OpenAI Chat Completion API endpoint."
  "https://api.openai.com/v1/chat/completions")
(comment
  ;; Example: Print the API URL.
  (println openai-api-url))

;; -----------------------------------------------------------------------------
;; JSON Response Parsing and Validation
;; -----------------------------------------------------------------------------

(defn parse-json-response
  "Parses the string `s` as JSON and returns the resulting Clojure map (with keyword keys).
   If parsing fails, returns the original string.
   
   Example:
   (parse-json-response \"{\\\"data\\\": [1,2,3], \\\"layout\\\": {\\\"title\\\": \\\"Line Chart\\\"}}\")"
  [s]
  (try
    (json/parse-string s true)
    (catch Exception e
      (println "Warning: Failed to parse JSON:" (.getMessage e))
      s)))
(comment
  ;; Example call:
  (parse-json-response "{\"data\": [1,2,3], \"layout\": {\"title\": \"Line Chart\"}}"))

;; Load the official Plotly JSON schema from the resources folder.
(def plotly-schema
  "The official Plotly JSON schema loaded from resources.
   Ensure that the file 'plotly-option.schema.json' is present in your resources folder."
  (with-open [r (io/reader (io/resource "plotly-option.schema.json"))]
    (let [schema-json (JSONObject. (JSONTokener. r))]
      (SchemaLoader/load schema-json))))
(comment
  ;; Example: Print the loaded schema (or check its type).
  (println (type plotly-schema)))

(defn validate-plot-spec
  "Validates the given plot-spec map against the official Plotly JSON schema.
   Returns true if valid; otherwise, prints the validation error and returns false.
   
   Example:
   (validate-plot-spec {:data [{:x [1 2 3] :y [4 5 6] :type \"scatter\"}]
                         :layout {:title \"Line Chart\"}})"
  [plot-spec]
  (let [json-str (json/generate-string plot-spec)
        json-obj (JSONObject. json-str)]
    (try
      (.validate plotly-schema json-obj)
      true
      (catch Exception e
        (println "Validation error:" (.getMessage e))
        false))))
(comment
  ;; Example call:
  (validate-plot-spec {:data [{:x [1 2 3] :y [4 5 6] :type "scatter"}]
                       :layout {:title "Line Chart"}}))

;; -----------------------------------------------------------------------------
;; chat-completion
;; -----------------------------------------------------------------------------

(defn chat-completion
  "Sends the conversation history (a vector of message maps) to the OpenAI API
   using GPT‑4 and returns the assistant’s reply parsed as JSON.
   
   Example:
   (chat-completion [{:role \"system\" :content \"You output JSON only.\"}
                     {:role \"user\" :content \"Output a JSON object: {\\\"data\\\": [{\\\"x\\\": [1,2,3], \\\"y\\\": [4,5,6], \\\"type\\\": \\\"scatter\\\"}], \\\"layout\\\": {\\\"title\\\": \\\"Line Chart\\\"}}\"}])"
  [messages]
  (let [payload {:model    "gpt-4"
                 :messages messages}]
    (try
      (let [response (client/post openai-api-url
                                  {:headers {"Authorization" (str "Bearer " api-key)
                                             "Content-Type"  "application/json"}
                                   :body    (json/generate-string payload)
                                   :as      :json})
            reply (-> response :body :choices first :message :content)]
        (parse-json-response reply))
      (catch Exception e
        (str "Error during API call: " (.getMessage e))))))
(comment
  ;; Example call:
  (chat-completion [{:role "system" :content "You output JSON only."}
                    {:role "user" :content "Output a JSON object: {\"data\": [{\"x\": [1,2,3], \"y\": [4,5,6], \"type\": \"scatter\"}], \"layout\": {\"title\": \"Line Chart\"}}"}]))

;; -----------------------------------------------------------------------------
;; Conversation & Plot History
;; -----------------------------------------------------------------------------

(def initial-system-message
  "The initial system message for every plot command."
  {:role "system"
   :content (str "You are an AI agent that generates Plotly plots in JSON format. "
                 "Given an EDN data structure and plot instructions, produce a valid Plotly plot "
                 "specification as a JSON object that conforms to the official Plotly JSON schema. "
                 "Return only a valid JSON object with no additional commentary or markdown formatting.")})

;; The conversation is reset for each new command.
(def conversation
  "An atom holding the current conversation history for a command.
   (Each new submission resets this to the initial system message.)"
  (atom [initial-system-message]))
(comment
  ;; Example: Print the current conversation.
  (println @conversation))

(def plot-history
  "An atom holding the history of generated plots.
   Each entry is a map with keys :data, :instruction, and :plot-spec."
  (atom []))
(comment
  ;; Example: Print the plot history.
  (println @plot-history))

;; -----------------------------------------------------------------------------
;; Process Command
;; -----------------------------------------------------------------------------

(defn process-command
  "Resets the conversation, sends the user data and instructions to the LLM,
   validates the output, and if valid, adds a new plot to the plot-history.
   Returns the new plot entry (a map with :data, :instruction, and :plot-spec)
   or an error string.
   
   Example:
   (process-command \"{:x [1 2 3] :y [4 5 6]}\" \"Create a scatter plot.\")"
  [data instruction]
  (reset! conversation [initial-system-message])
  (let [command (str "Data: " data "\nInstructions: " instruction)]
    (swap! conversation conj {:role "user" :content command})
    (let [plot-spec (chat-completion @conversation)
          valid?    (validate-plot-spec plot-spec)]
      (if valid?
        (let [entry {:data data :instruction instruction :plot-spec plot-spec}]
          (swap! plot-history conj entry)
          entry)
        (str "Invalid plot specification generated: " plot-spec)))))
(comment
  ;; Example call:
  (process-command "{:x [1 2 3] :y [4 5 6]}" "Create a scatter plot."))

;; -----------------------------------------------------------------------------
;; Render a Single Plot Entry
;; -----------------------------------------------------------------------------

(defn render-plot-entry
  "Renders a single plot entry as HTML.
   Each entry shows the data and instructions used and renders the Plotly plot with
   a collapsible JSON code block. A unique container ID is generated based on the index.
   
   Example:
   (render-plot-entry 0 {:data \"{:x [1 2 3] :y [4 5 6]}\" 
                         :instruction \"Create a scatter plot.\"
                         :plot-spec {:data [{:x [1 2 3] :y [4 5 6] :type \"scatter\"}],
                                     :layout {:title \"Line Chart\"}}})"
  [idx entry]
  (let [container-id (str "plot-" idx)
        plot-spec (:plot-spec entry)]
    [:div {:style "margin-bottom: 2em; border-bottom: 1px solid #ccc; padding-bottom: 1em;"}
     [:h3 (str "Plot " (inc idx))]
     [:p [:strong "Data: "] (:data entry)]
     [:p [:strong "Instruction: "] (:instruction entry)]
     ;; Container for the Plotly plot
     [:div {:id container-id :style "width:100%;height:400px; margin:1em 0;"}]
     ;; Collapsible JSON code block
     [:details
      [:summary "Show/Hide JSON Specification"]
      [:pre (json/generate-string plot-spec {:pretty true})]]
     ;; Inline JavaScript to render the plot using Plotly in this container
     [:script (str "var plotSpec = " (json/generate-string plot-spec) ";"
                   "Plotly.newPlot('" container-id "', plotSpec.data, plotSpec.layout);")]]))
(comment
  ;; Example call:
  (render-plot-entry 0 {:data "{:x [1 2 3] :y [4 5 6]}"
                        :instruction "Create a scatter plot."
                        :plot-spec {:data [{:x [1 2 3] :y [4 5 6] :type "scatter"}]
                                    :layout {:title "Line Chart"}}}))

;; -----------------------------------------------------------------------------
;; Render Gallery
;; -----------------------------------------------------------------------------

(defn render-gallery
  "Renders the gallery of all generated plots.
   Each plot is rendered using render-plot-entry.
   
   Example:
   (render-gallery)"
  []
  (if (empty? @plot-history)
    [:div "No plots generated yet."]
    [:div
     (for [[idx entry] (map-indexed vector @plot-history)]
       (render-plot-entry idx entry))]))
(comment
  ;; Example call:
  (render-gallery))

;; -----------------------------------------------------------------------------
;; Render Command Form
;; -----------------------------------------------------------------------------

(defn render-command-form
  "Returns the HTML form for entering the plot data and instructions.
   Uses HTMX for asynchronous submission and includes default values.
   
   Example:
   (render-command-form)"
  []
  [:form {:hx-post "/submit"
          :hx-target "#gallery"
          :hx-indicator "#loading"
          :method "post"}
   [:div
    [:label "Data (EDN):"]
    [:br]
    ;; The default value is provided as the initial content of the textarea.
    [:textarea {:name "data" :rows 4} "{:x [1 2 3] :y [4 5 6]}"]]
   [:div
    [:label "Instructions:"]
    [:br]
    [:textarea {:name "instruction" :rows 4} "Create a scatter plot."]]
   [:div
    [:input {:type "submit" :value "Submit Command" :style "margin-top: 1em;"}]]])
(comment
  ;; Example call:
  (render-command-form))

;; -----------------------------------------------------------------------------
;; Layout (using Hiccup and HTMX)
;; -----------------------------------------------------------------------------

(defn layout
  "Wraps content in a basic HTML5 layout.
   Loads Plotly.js and HTMX from CDNs.
   
   Example:
   (layout \"My Title\" [:p \"Hello world!\"])"
  [title & content]
  (html5
   [:head
    [:meta {:charset "UTF-8"}]
    [:title title]
    ;; Load Plotly.js and HTMX
    [:script {:src "https://cdn.plot.ly/plotly-latest.min.js"}]
    [:script {:src "https://unpkg.com/htmx.org@1.9.2"}]
    [:style "
      body { font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; margin: 2em; background: #f9f9f9; }
      .container { max-width: 800px; margin: auto; background: white; padding: 1em; box-shadow: 0 2px 8px rgba(0,0,0,0.1); }
      textarea { width: 100%; font-family: monospace; }
      pre { background-color: #f4f4f4; padding: 1em; overflow: auto; }
      details summary { cursor: pointer; font-weight: bold; }
      .spinner { display: none; }
    "]]
   [:body
    [:div.container
     [:h1 title]
     ;; HTMX loading indicator
     [:div#loading.spinner "Loading..."]
     content]]))
(comment
  ;; Example call:
  (layout "Test Page" [:p "Hello world!"]))

;; -----------------------------------------------------------------------------
;; Web Routes and Application
;; -----------------------------------------------------------------------------

(defroutes app-routes
  (GET "/" []
       (layout "Plotly Plot Generator"
               (render-command-form)
               [:div#gallery (render-gallery)]))
  (POST "/submit" {params :params}
        (let [data        (get params "data")
              instruction (get params "instruction")]
          (process-command data instruction)
          ;; Return the updated gallery (this will replace the #gallery div via HTMX)
          (html5 [:body (render-gallery)])))
  (route/not-found "Page not found"))
(comment
  ;; Example: To test routes directly, start the server and visit http://localhost:3000.
  )

(def app
  "The Ring application wrapped with parameters middleware.
   Example: (app)"
  (wrap-params app-routes))
(comment
  ;; Example call:
  (app))

;; -----------------------------------------------------------------------------
;; Main Entry Point
;; -----------------------------------------------------------------------------

(defn -main
  "Starts the web server on port 3000.
   Example: (-main)
   Then open http://localhost:3000 in your browser."
  [& args]
  (run-jetty app {:port 3000 :join? false}))
(comment
  ;; Example call to start the server:
  (-main))
