
function Context() {
    var instanceVariables = { };
    var localVariables = { };
    
    this.defineInstanceVariable = function (name) {
        instanceVariables[name] = true;
    }
    
    this.isInstanceVariable = function (name) {
        if (instanceVariables[name])
            return true;
            
        return false;
    }
    
    this.defineLocalVariable = function (name) {
        localVariables[name] = true;
    }
    
    this.isLocalVariable = function (name) {
        if (localVariables[name])
            return true;
            
        return false;
    }
}

function createContext() {
    return new Context();
}

module.exports = {
    createContext: createContext
};