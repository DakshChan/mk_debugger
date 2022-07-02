var express = require('express');
var router = express.Router();
const { spawn } = require('child_process');

const fs = require('fs');
const testdata = fs.readFileSync("./sample_response.json")

/* GET users listing. */
router.get('/', function(req, res, next) {
  const racket = spawn('racket.exe', [], {shell: true});
  // const data = req.body; // or something so we can run the racket code required
  // racket.stdin.write(data);
  // racket.stdout.on('data', (data) => {
  //   res.send(data.toString());
  // });;
  // racket.stderr.on('data', (data) => {
  //   res.send(data.toString());
  // });
  // racket.disconnect();
  res.status(200).json(testdata);
});

module.exports = router;
