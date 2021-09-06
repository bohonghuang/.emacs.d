((magit-branch nil)
 (magit-commit nil
               ("--allow-empty"))
 (magit-diff
  (("--" "init.el"))
  (("--" "org-sketch.el")
   "--no-ext-diff" "--stat"))
 (magit-dispatch nil)
 (magit-log
  ("-n256"
   ("--" "logic/src/main/scala/org/coco24/pmgame/battle/mode/normal/ai/AINormalBattleClient.scala")
   "--graph" "--decorate")
  ("-n256"
   ("--" "init.el")
   "--graph" "--decorate"))
 (magit-merge nil
              ("--ff-only"))
 (magit-pull nil)
 (magit-push nil
             ("--force"))
 (magit-rebase nil)
 (magit-remote
  ("-f"))
 (magit-remote\.<remote>\.*url "git@github.com:HuangBoHong/PokemonEonianEmeraldMaps.git" "https://github.com/HuangBoHong/PokemonEonianEmeraldMaps.git" "git@github.com:HuangBoHong/org-sketch.git" "https://github.com/yuchen-lea/org-sketch.git" "git@github.com:HuangBoHong/org-mode-notes.git" "https://github.com/HuangBoHong/org-mode-notes.git" "git@github.com:HuangBoHong/Rov.git" "https://github.com/HuangBoHong/Rov.git")
 (magit-submodule
  ("--force")
  nil)
 (magit:-- "logic/src/main/scala/org/coco24/pmgame/battle/mode/normal/ai/AINormalBattleClient.scala" "" "init.el" "org-sketch.el"))
