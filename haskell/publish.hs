import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.IO
import System.Process
import System.Exit (exitFailure)
import Data.Text (Text, pack, strip, unpack)

import Su.Date
import Su.Utils

main = do
  args <- getArgs
  case args of
    mode:custom_id:entered_on:heading_:hasMath:tags -> do
      enteredOn <- fromOrgDateGetLocal entered_on
      (_,_,_,process) <- createProcess (proc "/usr/bin/ssh"
                                        (["chaos",
                                          "cd ~/virtualEnvs/blog/src/blog;../../.virthualenv/cabal/bin/publish-entry" ,
                                          quoteArgs mode,
                                          quoteArgs custom_id,
                                          quoteArgs enteredOn,
                                          quoteArgs heading_,
                                          quoteArgs hasMath]
                                         ++ tags)
                                       )
      waitForProcess process

    _ -> do
      name <- getProgName
      hPutStrLn stderr $ "usage: " ++ name ++ " customID enteredOn heading [tags]"
      exitFailure
