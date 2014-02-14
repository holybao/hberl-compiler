
###An Erlang translator wirh code security check written in Erlang

####Exported functions:

* hberl_compiler:compile(File)
* hberl_compiler:compile(File, Options)
* hberl_compiler:compile2file(SourceFile,DestFile)
* hberl_compiler:compile2file(SourceFile,DestFile, Options)
* hberl_compiler:compile2forms(File)

Ex. hberl_compiler:compile2files("test/src.erl","test/dest.erl",[{module,"dest"}]).
