(ns onteater.version
  "Single source of truth for the app version. `shadow.resource/inline` reads
  src/onteater/version.txt at compile time and inlines it as a string literal,
  so no runtime filesystem access is needed and it survives :advanced. Editing
  version.txt hot-reloads in `npm run dev` (shadow watches the inlined file)."
  (:require [clojure.string :as str]
            [shadow.resource :as rc]))

(def string (str/trim (rc/inline "onteater/version.txt")))
