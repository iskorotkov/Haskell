relatives []  person = []
relatives all person = relatives' all [] [person]
 where
  --relatives'::[(String, String)] -> [String] -> [String] -> [String]
  relatives' all rels [] = rels
  relatives' all rels sons = relatives' all (rels++[s|(f,s)<-all, f `elem` sons]) [s|(f,s)<-all, f `elem` sons]
