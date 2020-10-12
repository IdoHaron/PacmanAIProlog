var pl = require("tau-prolog");
require("tau-prolog/modules/lists")(pl);

const hbs = require('hbs');
const path = require('path');

const express = require('express');
const app = express();
const port = process.env.PORT || 3000;

//Define public ,views and partials path
const publicPath = path.join(__dirname, './public');
const viewsPath = path.join(__dirname, './views')

app.set('views', viewsPath)
app.use(express.static(publicPath));

//Setup the hbs engine
app.set('view engine', 'hbs');

app.get('/', (req, res) => {
  res.render('doge');
});

app.listen(port, () => {
  console.log(`Example app listening at http://localhost:${port}`);
});