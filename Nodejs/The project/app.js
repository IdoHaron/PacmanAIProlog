const http = require("http");
const express = require("express");
const app = express();
const server = http.createServer(app);
const io = require("socket.io")(server);
//#endregion 
app.set('views', './views');
app.set('view engine', 'ejs');
app.use(express.static('public'));
app.use(express.urlencoded({
    extended: true
}));

app.get("/", (req, res) => {
    res.render("home");
});

io.on("connection", (socket) => {
    socket.emit("hello world");
});
server.listen(3000);