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
   If parsing fails, it returns the original string.
   
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
  (with-open [r (io/reader (io/resource "plot-schema.json"))]
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
  (generate-initial-plot "{\"data\": [{\"x\": [1,2,3], \"y\": [4,5,6], \"type\": \"scatter\"}], \"layout\": {\"title\": \"Line Chart\"}}"
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
;; Web UI Helpers (using Hiccup)
;; -----------------------------------------------------------------------------

(defn layout
  "Wraps content in a basic HTML5 layout and loads Plotly.js from the CDN.
   
   Example:
   (layout \"My Title\" [:p \"Hello world!\"])"
  [title & content]
  (html5
   [:head
    [:meta {:charset "UTF-8"}]
    [:title title]
    ;; Load Plotly.js from the official CDN.
    [:script {:src "https://cdn.plot.ly/plotly-latest.min.js"}]
    [:style "body { font-family: sans-serif; margin: 2em; } 
             textarea { width: 100%; } 
             pre { background-color: #f4f4f4; padding: 1em; }"]]
   [:body
    [:h1 title]
    content]))
(comment
  ;; Example call:
  (layout "Test Page" [:p "Hello world!"]))

(defn render-initial-form
  "Returns the HTML form for entering initial data and plot instructions.
   
   Example:
   (println (render-initial-form))"
  []
  [:div
   [:form {:action "/generate" :method "post"}
    [:div
     [:label "Initial Data (JSON):"]
     [:br]
     [:textarea {:name "data" :rows 4 :cols 50 :placeholder "{\"data\": [{\"x\": [1,2,3], \"y\": [4,5,6], \"type\": \"scatter\"}], \"layout\": {\"title\": \"Line Chart\"}}"}]]
    [:div
     [:label "Plot Instructions:"]
     [:br]
     [:textarea {:name "instruction" :rows 4 :cols 50 :placeholder "Create a line chart."}]]
    [:div [:input {:type "submit" :value "Generate Plot"}]]]])
(comment
  ;; Example call:
  (render-initial-form))

(defn render-update-form
  "Returns the HTML form for entering follow‑up instructions.
   
   Example:
   (println (render-update-form))"
  []
  [:div
   [:form {:action "/update" :method "post"}
    [:div
     [:label "Update Instruction:"]
     [:br]
     [:textarea {:name "instruction" :rows 4 :cols 50 :placeholder "e.g., Change the marker color."}]]
    [:div [:input {:type "submit" :value "Update Plot"}]]]])
(comment
  ;; Example call:
  (render-update-form))

(defn render-result
  "Returns HTML displaying the generated Plotly plot specification (as JSON)
   and renders the Plotly plot.
   
   It creates a container for the plot and an inline script that calls Plotly.newPlot.
   
   Example:
   (render-result {:data [{:x [1,2,3], :y [4,5,6], :type \"scatter\"}],
                   :layout {:title \"Line Chart\"}})"
  [plot-spec]
  (let [plot-json (json/generate-string plot-spec)]
    [:div
     [:h2 "Generated Plotly Plot Specification (JSON)"]
     [:pre (json/generate-string plot-spec {:pretty true})]
     ;; Container for the plot.
     [:div {:id "plot" :style "width:100%;height:400px;"}]
     ;; Inline JavaScript to render the plot using Plotly.
     [:script (str "var plotSpec = " plot-json ";"
                   "Plotly.newPlot('plot', plotSpec.data, plotSpec.layout);")]]))
(comment
  ;; Example call:
  (render-result {:data [{:x [1,2,3], :y [4,5,6], :type "scatter"}],
                  :layout {:title "Line Chart"}}))

;; -----------------------------------------------------------------------------
;; Web Routes and Application
;; -----------------------------------------------------------------------------

(defroutes app-routes
  (GET "/" []
       (layout "Plotly Plot Generator" (render-initial-form)))
  (POST "/generate" {params :params}
        (let [data        (get params "data")
              instruction (get params "instruction")
              plot-spec   (generate-initial-plot data instruction)]
          (layout "Plot Generated"
                  (render-result plot-spec)
                  (render-update-form))))
  (POST "/update" {params :params}
        (let [instruction (get params "instruction")
              plot-spec   (update-plot instruction)]
          (layout "Plot Updated"
                  (render-result plot-spec)
                  (render-update-form))))
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
