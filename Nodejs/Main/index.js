var express = require('express');
const app = express();
var http = require('http').createServer(app);
var io = require('socket.io')(http);
let p = require("./node_modules/tau-prolog");

let session = p.create(1000);
app.set('views', './views');
app.set('view engine', 'ejs');
app.use(express.static('public'));
app.use(express.static(__dirname + '/public'));
app.use(express.urlencoded({
    extended: true
}));
//#endregion

//#region page_manage
app.get('/', (req, res) => {
    res.render('index');
}).listen(7000);

io.on("connection", (socket)=>{
    socket.on("Ghost Move", (board, x,y)=>{
        board[x][y] = 2;
        return session.query(`move_ghost(${board}, Move).`);
    })
})

