(ns onteater.ui.markdown
  "Markdown + LaTeX-math rendering for scenarios. Uses markdown-it for
  Markdown and KaTeX for `$…$` / `$$…$$` math, both vendored (KaTeX fonts are
  inlined as base64 by the single-file build). Math is rendered to HTML first and
  swapped in via placeholder tokens so markdown-it does not mangle it; KaTeX runs
  with throwOnError=false so malformed math degrades to visible source rather than
  breaking the page."
  (:require [clojure.string :as str]
            ["markdown-it" :as MarkdownIt]
            ["katex" :as katex]))

;; typographer is OFF on purpose: it would rewrite quotes/dashes in the rendered
;; text (' -> ', -- -> —), breaking exact-excerpt matching against the LLM's quotes.
(defonce ^:private md
  (MarkdownIt. #js {:html false :linkify true :typographer false :breaks false}))

(defn- render-math [tex display?]
  (try
    (.renderToString katex tex #js {:displayMode display? :throwOnError false})
    (catch :default _ (str (if display? "$$" "$") tex (if display? "$$" "$")))))

(defn- extract-math
  "Replace $$…$$ and $…$ spans in `text` with placeholder tokens, returning
  [text-with-placeholders token->html]. Block math is handled before inline. The
  token uses a unicode sentinel unlikely to occur in scenarios and untouched by
  markdown-it/typographer, so the swap-back is exact."
  [text]
  (let [store (atom {})
        n     (atom 0)
        stash (fn [html]
                (let [tok (str "❖MATH" (swap! n inc) "❖")]
                  (swap! store assoc tok html) tok))
        block (str/replace text #"\$\$([\s\S]+?)\$\$"
                           (fn [[_ tex]] (stash (render-math (str/trim tex) true))))
        inline (str/replace block #"(?<!\$)\$(?!\$)([^\$\n]+?)\$(?!\$)"
                            (fn [[_ tex]] (stash (render-math (str/trim tex) false))))]
    [inline @store]))

(defn render-html
  "Render `text` (Markdown + math) to an HTML string suitable for
  dangerouslySetInnerHTML."
  [text]
  (let [[prepared token->html] (extract-math (or text ""))
        html (.render md prepared)]
    (reduce-kv (fn [h tok m] (str/replace h tok m)) html token->html)))
