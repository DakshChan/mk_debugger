const express = require('express');
const path = require('path');
const cors = require('cors');

const { spawn } = require('child_process');
const fs = require('fs');
const testdata = JSON.parse(fs.readFileSync("./sample_response.json"));

const app = express();
const port = process.env.PORT || 3000;

const formData = require('express-form-data');

app.use(formData.parse());

app.use(express.static(path.join(__dirname, 'build')))

app.use(cors({
  origin: '*'
}));

app.get('/code', (req, res) => {
  res.send(testdata);
})

let userCode = "";
app.post('/code', (req, res) => {
  //TODO: reformat before sending back to client
  userCode = fs.readFileSync(req.files.file.path).toString();
  // res.send(userCode);
  res.sendFile(req.files.file.path)
});

// app.get('(/*)?', (req, res) => {
//   res.sendFile(path.join(__dirname, 'build', 'index.html'));
// });

app.listen(port, () => {
  console.log(`Example app listening on port ${port}`)
})
