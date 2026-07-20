(ns onteater.ui.chat
  "Chat drawer: a per-session transcript with the mapping context, quick
  actions, and — the distinctive part — LLM-proposed mapping changes surfaced as
  reviewable diff cards. Nothing auto-applies; the user clicks Apply/Dismiss per op,
  and applied ops are undoable. Ops touching forced entries are rejected with a
  visible notice."
  (:require [re-frame.core :as rf]
            [onteater.ui.markdown :as markdown]))

(defn- md [content]
  [:div.msg-md {:dangerouslySetInnerHTML #js {:__html (markdown/render-html (or content ""))}}])

(defn- op-line [{:keys [op target value] :as o}]
  ;; One diff-card renderer for entries AND timeline events/relations (§6.5).
  (let [target (or target :entry)
        verb   (case op :add "add" :update "update" :remove "remove" (name op))
        v      (or value (:entry o))
        [subject arrow-target]
        (case target
          :event    [(str "event “" (:label v) "”") (or (:node-id v) "(untyped)")]
          :relation [(str "relation " (some-> (:type v) name)) (:property-id v)]
          [(str "“" (:excerpt v) "”") (:node-id v)])]
    [:div.op-line
     [:span.op-verb {:class (str "op-" (name op))} verb]
     [:span.op-target-tag (name target)]
     [:span.op-excerpt subject]
     (when (and (not= :remove op) arrow-target)
       [:<> [:span.op-arrow "→"] [:span.op-node arrow-target]])]))

(defn- op-card [msg-id ui-idx op-idx {:keys [state] :as op}]
  [:div.op-card {:class (str "op-" (name (or state :pending)))}
   [op-line op]
   (case state
     :applied  [:span.op-status.op-applied "✓ applied"]
     :dismissed [:span.op-status.op-dismissed "dismissed"]
     :rejected [:span.op-status.op-rejected "⚠ blocked (forced entry)"]
     [:div.op-actions
      [:button.btn.btn-sm.btn-primary
       {:on-click #(rf/dispatch [:chat/apply-op msg-id ui-idx op-idx])} "Apply"]
      [:button.btn.btn-sm
       {:on-click #(rf/dispatch [:chat/dismiss-op msg-id ui-idx op-idx])} "Dismiss"]])])

(defn- update-block [msg-id ui-idx {:keys [ops reason]}]
  [:div.update-block
   [:div.update-head "Proposed change"
    (when (> (count ops) 1)
      [:button.btn.btn-sm {:on-click #(rf/dispatch [:chat/apply-all msg-id ui-idx])} "Apply all"])]
   (when (seq reason) [:div.update-reason reason])
   (for [[op-idx op] (map-indexed vector ops)]
     ^{:key op-idx} [op-card msg-id ui-idx op-idx op])])

(defn- message [{:keys [id role content updates pending?]}]
  [:div.chat-msg {:class (str "msg-" (name role))}
   [:div.msg-role (case role :user "You" :assistant "Assistant" (name role))]
   (if pending?
     [:div.msg-thinking [:span.thinking-dots "● ● ●"] " thinking…"]
     [:div.msg-body
      (when (seq content) [md content])
      (for [[ui-idx u] (map-indexed vector updates)]
        ^{:key ui-idx} [update-block id ui-idx u])])])

(defn- quick-actions []
  (let [tab @(rf/subscribe [:timeline/tab])]
    [:div.quick-actions
     (if (= :mapping (or tab :mapping))
       [:<>
        [:button.chip.chip-sm {:on-click #(rf/dispatch [:chat/quick-action :explain])} "Explain selection"]
        [:button.chip.chip-sm {:on-click #(rf/dispatch [:chat/quick-action :alternatives])} "Alternatives"]
        [:button.chip.chip-sm {:on-click #(rf/dispatch [:chat/quick-action :low-confidence])} "Re-examine low-conf"]
        [:button.chip.chip-sm {:on-click #(rf/dispatch [:chat/quick-action :unmapped])} "What's unmapped?"]]
       ;; Timeline/Gaps tabs: grounded dependency questions (§6.5 / §12.14).
       [:<>
        [:button.chip.chip-sm {:on-click #(rf/dispatch [:chat/timeline-action :depends])} "What does this depend on?"]
        [:button.chip.chip-sm {:on-click #(rf/dispatch [:chat/timeline-action :trace])} "Trace the chain"]
        [:button.chip.chip-sm {:on-click #(rf/dispatch [:chat/timeline-action :why-untyped])} "Why untyped?"]])]))

(defn- composer []
  (let [input @(rf/subscribe [:chat/input])
        pending? @(rf/subscribe [:chat/pending?])
        send! (fn [] (let [v (or input "")]
                       (when (seq (.trim v)) (rf/dispatch [:chat/send v]))))]
    [:div.chat-composer
     [:textarea.chat-input
      {:value (or input "")
       :placeholder "Ask about this mapping…  (Enter to send, Shift+Enter for newline)"
       :on-change #(rf/dispatch [:chat/set-input (.. % -target -value)])
       :on-key-down (fn [e]
                      (when (and (= "Enter" (.-key e)) (not (.-shiftKey e)))
                        (.preventDefault e) (send!)))}]
     [:button.btn.btn-primary {:disabled pending? :on-click send!} "Send"]]))

(defn drawer []
  (let [msgs @(rf/subscribe [:chat/messages])]
    [:div.chat-drawer
     [:div.chat-head
      [:span "Chat"]
      [:div.chat-head-actions
       [:button.icon-btn {:title "Undo mapping change"
                          :disabled (not @(rf/subscribe [:mapping/can-undo?]))
                          :on-click #(rf/dispatch [:mapping/undo])} "↶"]
       [:button.icon-btn {:on-click #(rf/dispatch [:chat/toggle])} "×"]]]
     [quick-actions]
     [:div.chat-log
      (if (seq msgs)
        (for [m msgs] ^{:key (:id m)} [message m])
        [:div.chat-empty
         [:p "Ask why an element was mapped, request alternatives, or have the model
              propose changes — you review each as a diff before it applies."]])]
     [composer]]))
