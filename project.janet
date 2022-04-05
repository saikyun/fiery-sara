(declare-project
  :name "Sara Flames"
  :author "Jona Ekenberg <saikyun@gmail.com>"
  :dependencies [## using my own fork due to additions to jaylib
                 "https://github.com/saikyun/freja-jaylib"

                 # for vector math
                 "https://github.com/saikyun/freja"])

(declare-executable
  :name "Sara Flames"
  :entry "fire.janet")
