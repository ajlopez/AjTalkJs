
function Context() {
    var locals = { };
    
    this.defineLocal = function (name) {
        locals[name] = true;
    }
    
    this.isLocal = function (name) {
        if (locals[name])
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