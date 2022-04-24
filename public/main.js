const app = Elm.Main.init()

app.ports.persistPush.subscribe(session => {
    localStorage.setItem("keyboard-collector", JSON.stringify(session))
})
app.ports.persistClear.subscribe(session => {
    localStorage.removeItem("keyboard-collector")
})
app.ports.persistRequest.subscribe(() => {
    const session = localStorage.getItem("keyboard-collector")
    app.ports.persistPull.send(session ? JSON.parse(session) : null)
})