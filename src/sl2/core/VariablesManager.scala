package sl2.core

object VariablesManager {

    val VAR_PREFIX = "var_";
    var VAR_INDEX = 0;

    def getNewName() : String = {
	    VAR_INDEX += 1;
	    VAR_PREFIX + VAR_INDEX
    }
}