
function Context() {
    var instanceVariables = { };
    
    this.defineInstanceVariable = function (name) {
        instanceVariables[name] = true;
    }
    
    this.isInstanceVariable = function (name) {
        if (instanceVariables[name])
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