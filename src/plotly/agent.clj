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
;; Conversation Atom
;; -----------------------------------------------------------------------------

(def conversation
  "An atom holding the conversation history.
   The system message instructs the AI to output a pure JSON object that conforms
   to the official Plotly JSON schema (with no extra commentary)."
  (atom
   [{:role "system"
     :content (str "You are an AI agent that generates Plotly plots in JSON format. "
                   "Given data and textual instructions, produce a valid Plotly plot "
                   "specification as a JSON object that conforms to the official Plotly JSON schema. "
                   "Return only a valid JSON object with no additional commentary or markdown formatting.")}]))
(comment
  ;; Example: Print the current conversation history.
  (println @conversation))

;; -----------------------------------------------------------------------------
;; render-conversation-history
;; -----------------------------------------------------------------------------

(defn render-conversation-history
  "Renders the conversation history as a collapsible HTML block.
   
   Example:
   (render-conversation-history)"
  []
  (let [msgs @conversation]
    [:details {:open true :style "margin: 1em 0;"}
     [:summary "Show Conversation History"]
     (for [{:keys [role content]} msgs]
       [:div {:style "margin: 0.5em 0; padding: 0.5em; border-bottom: 1px solid #ddd;"}
        [:strong (str role ": ")]
        [:span content]])]))
(comment
  ;; Example call:
  (render-conversation-history))

;; -----------------------------------------------------------------------------
;; generate-initial-plot
;; -----------------------------------------------------------------------------

(defn generate-initial-plot
  "Adds the initial user message (data and instructions) to the conversation,
   calls the API, validates the output against the official Plotly JSON schema,
   and returns the generated plot specification as a map.
   
   Parameters:
   - data: The data (in JSON, EDN, or plain text) for the plot.
   - instruction: A textual instruction describing the desired plot.
   
   Example:
   (generate-initial-plot \"{\\\"data\\\":[{\\\"x\\\": [1,2,3], \\\"y\\\": [4,5,6], \\\"type\\\": \\\"scatter\\\"}], \\\"layout\\\": {\\\"title\\\": \\\"Line Chart\\\"}}\" \"Create a line chart.\")"
  [data instruction]
  (let [user-message (str "Data: " data "\nInstructions: " instruction)]
    (swap! conversation conj {:role "user" :content user-message})
    (let [plot-spec (chat-completion @conversation)
          valid?    (validate-plot-spec plot-spec)]
      (if valid?
        (do
          (swap! conversation conj {:role "assistant" :content (json/generate-string plot-spec)})
          plot-spec)
        (str "Invalid plot specification generated: " plot-spec)))))
(comment
  ;; Example call:
  (generate-initial-plot "{:x [1 2 3] :y [4 5 6]}"
                         "Create a line chart."))

;; -----------------------------------------------------------------------------
;; update-plot
;; -----------------------------------------------------------------------------

(defn update-plot
  "Adds a follow‑up instruction to the conversation, calls the API,
   validates the output against the official Plotly JSON schema, and returns
   the updated plot specification as a map.
   
   Parameter:
   - instruction: A textual instruction for modifying the plot.
   
   Example:
   (update-plot \"Please change the marker color to blue.\")"
  [instruction]
  (swap! conversation conj {:role "user" :content instruction})
  (let [plot-spec (chat-completion @conversation)
        valid?    (validate-plot-spec plot-spec)]
    (if valid?
      (do
        (swap! conversation conj {:role "assistant" :content (json/generate-string plot-spec)})
        plot-spec)
      (str "Invalid plot specification generated: " plot-spec))))
(comment
  ;; Example call:
  (update-plot "Please change the marker color to blue."))

;; -----------------------------------------------------------------------------
;; Web UI Helpers (using Hiccup and HTMX)
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
      .message { margin: 0.5em 0; padding: 0.5em; border-bottom: 1px solid #ddd; }
      .message strong { color: #333; }
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

(defn render-initial-form
  "Returns the HTML form for entering initial data and plot instructions.
   Uses HTMX attributes for asynchronous submission.
   
   Example:
   (render-initial-form)"
  []
  [:form {:hx-post "/generate"
          :hx-target "#result"
          :hx-indicator "#loading"
          :method "post"}
   [:div
    [:label "Initial Data (EDN):"]
    [:br]
    [:textarea {:name "data"
                :rows 4
                :placeholder "{:x [1 2 3] :y [4 5 6]}"}]]
   [:div
    [:label "Plot Instructions:"]
    [:br]
    [:textarea {:name "instruction" :rows 4 :placeholder "Create a line chart."}]]
   [:div
    [:input {:type "submit" :value "Generate Plot" :style "margin-top: 1em;"}]]])
(comment
  ;; Example call:
  (render-initial-form))

(defn render-update-form
  "Returns the HTML form for entering follow‑up instructions.
   Uses HTMX attributes for asynchronous submission.
   
   Example:
   (render-update-form)"
  []
  [:form {:hx-post "/update"
          :hx-target "#result"
          :hx-indicator "#loading"
          :method "post"}
   [:div
    [:label "Update Instruction:"]
    [:br]
    [:textarea {:name "instruction" :rows 4 :placeholder "e.g., Change the marker color."}]]
   [:div
    [:input {:type "submit" :value "Update Plot" :style "margin-top: 1em;"}]]])
(comment
  ;; Example call:
  (render-update-form))

(defn render-result
  "Returns HTML displaying the generated Plotly plot specification (as JSON),
   renders the Plotly plot, and includes a collapsible code block for the JSON.
   
   Example:
   (render-result {:data [{:x [1,2,3], :y [4,5,6], :type \"scatter\"}],
                   :layout {:title \"Line Chart\"}})"
  [plot-spec]
  (let [plot-json (json/generate-string plot-spec)]
    [:div
     [:h2 "Generated Plotly Plot Specification (JSON)"]
     ;; Collapsible code block
     [:details
      [:summary "Show/Hide JSON"]
      [:pre (json/generate-string plot-spec {:pretty true})]]
     ;; Container for the Plotly plot
     [:div {:id "plot" :style "width:100%;height:400px; margin:1em 0;"}]
     ;; Inline JavaScript to render the plot using Plotly
     [:script (str "var plotSpec = " plot-json ";"
                   "Plotly.newPlot('plot', plotSpec.data, plotSpec.layout);")]]))
(comment
  ;; Example call:
  (render-result {:data [{:x [1,2,3], :y [4,5,6], :type "scatter"}],
                  :layout {:title "Line Chart"}}))

(defn render-conversation-history
  "Renders the conversation history as a collapsible HTML block.
   
   Example:
   (render-conversation-history)"
  []
  (let [msgs @conversation]
    [:details {:open true :style "margin: 1em 0;"}
     [:summary "Show Conversation History"]
     (for [{:keys [role content]} msgs]
       [:div {:style "margin: 0.5em 0; padding: 0.5em; border-bottom: 1px solid #ddd;"}
        [:strong (str role ": ")]
        [:span content]])]))
(comment
  ;; Example call:
  (render-conversation-history))

(defn render-conversation-history-component
  "Renders the conversation history along with a button to refresh it via HTMX.
   The history is shown in a collapsible block.
   
   Example:
   (render-conversation-history-component)"
  []
  (let [history-html (render-conversation-history)]
    [:div
     [:h3 "Conversation History"]
     ;; HTMX refresh button that fetches the conversation history from /history.
     [:button {:hx-get "/history" :hx-target "#history-div"} "Refresh History"]
     [:div#history-div history-html]]))
(comment
  ;; Example call:
  (render-conversation-history-component))

;; -----------------------------------------------------------------------------
;; Web Routes and Application
;; -----------------------------------------------------------------------------

(defroutes app-routes
  (GET "/" []
       (layout "Plotly Plot Generator"
               (render-initial-form)
               (render-update-form)
               [:div#result]
               (render-conversation-history-component)))
  (POST "/generate" {params :params}
        (let [data        (get params "data")
              instruction (get params "instruction")
              plot-spec   (generate-initial-plot data instruction)]
          (layout "Plot Generated"
                  (render-result plot-spec)
                  (render-update-form)
                  (render-conversation-history-component))))
  (POST "/update" {params :params}
        (let [instruction (get params "instruction")
              plot-spec   (update-plot instruction)]
          (layout "Plot Updated"
                  (render-result plot-spec)
                  (render-update-form)
                  (render-conversation-history-component))))
  (GET "/history" []
       ;; Return just the conversation history block.
       (html5
        [:body (render-conversation-history)]))
  (route/not-found "Page not found"))
(comment
  ;; Example: To test routes directly, start the server and visit http://localhost:3000.
  )

(def app
  "The Ring application wrapped with parameters middleware.
   
   Example:
   (app)"
  (wrap-params app-routes))
(comment
  ;; Example call:
  (app))

;; -----------------------------------------------------------------------------
;; Main Entry Point
;; -----------------------------------------------------------------------------

(defn -main
  "Starts the web server on port 3000.
   
   Example:
   (-main)
   Then open http://localhost:3000 in your browser."
  [& args]
  (run-jetty app {:port 3000 :join? false}))
(comment
  ;; Example call to start the server:
  (-main))

