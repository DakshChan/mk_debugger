const express = require('express');
const path = require('path');
const {resolve} = require('path');
const cors = require('cors');
const tmp = require('tmp');
const formData = require('express-form-data');
const { spawn, spawnSync} = require('child_process');
const fs = require('fs');

const app = express();
const port = process.env.PORT || 3000;


app.use(formData.parse());

app.use(express.static(path.join(__dirname, 'build')))

// cors only being used when running react server, remove after react build
app.use(cors({
  origin: '*'
}));

let userRepl = null;
let racket = null;
let racketBuf = null;

app.post('/kill', (req, res) => {
  racket?.kill?.();
  racket = null;
  racketBuf = null;
  res.status(200).send('killed');
});

app.post('/code', (req, res) => {
  //TODO: potentially reformat before sending back to client
  let userCode = req.files.file.path;
  console.log(userCode);
  racket?.kill?.();
  racketBuf = null;

  console.log(resolve("../debugger/mk-fo.rkt").replaceAll("\\", "\\\\"));
  let tmpRepl = tmp.fileSync({postfix: '.rkt'});
  console.log(tmpRepl.name);
  let modifiedRepl = fs.readFileSync("repl.rkt").toString()
    .replace("{{$USER_CODE}}", userCode.replaceAll("\\", "\\\\"))
    .replace("{{$DEBUGGER_PATH}}", resolve("../debugger/mk-fo.rkt").replaceAll("\\", "\\\\"))
  fs.writeFileSync(tmpRepl.name, modifiedRepl);
  userRepl = tmpRepl.name;

  res.sendFile(userCode);
});

app.post('/debug', (req, res) => {
  console.log(JSON.stringify(req.body));

  if (userRepl === null) {
    res.status(400).send("No code uploaded");
    return;
  }

  racketBuf = "";
  racket?.stdout?.removeAllListeners?.();
  racket?.stderr?.removeAllListeners?.();

  if (req.body.command === "run") { // run
    racket?.kill?.();
    racket = spawn("racket", [userRepl]);
    // let d = spawnSync("bash", ["-c", "docker run -rm -it racket:8.7-full"]);
    // console.log(d?.stdout?.toString());
    // console.log(d?.stderr?.toString());

  } else { // resume
    if (racket === null) {
      res.status(400).send("No racket process running");
      return;
    }
  }

  racket.stdin.write(JSON.stringify(req.body) + "\n");

  racket.stdout.on('data', (data) => {
    // console.log(`$stdout: ${data}`);
    racketBuf += data;
    try {
      const resultJSON = JSON.parse(racketBuf);
      // console.log(JSON.stringify(resultJSON));
      resultJSON["program-points"] = resultJSON["program-points"].filter(x => x.syntax.source !== false)
      // resultJSON["rejected-states"] = resultJSON["rejected-states"].map(x => {x.path = x.path.filter(y => y.source !== false); return x;});
      // resultJSON["rejected-states"] = resultJSON["rejected-states"].map(x => {x.stack = x.stack.filter(y => y.source !== false); return x;});
      // resultJSON["solutions"] = resultJSON["solutions"].map(x => {x.path = x.path.filter(y => y.source !== false); return x;});
      // resultJSON["solutions"] = resultJSON["solutions"].map(x => {x.stack = x.stack.filter(y => y.source !== false); return x;});
      // console.log(JSON.stringify(resultJSON));
      res.json(resultJSON);
      res.end();
    } catch (e) {
      // ignore because incomplete json, keep buffering
    }
  });

  racket.stderr.on('data', (data) => {
    console.error(`stderr: ${data.toString()}`);
    res.status(400).write(data.toString());
    res.end();

    racket.stdout.removeAllListeners();
    racket.stderr.removeAllListeners();
    racket.kill();
    racket = null;
  });

  racket.on('exit', (code, signal) => {
    res.status(400).end();
  })
});

app.get('(/*)?', (req, res) => {
  res.sendFile(path.join(__dirname, 'build', 'index.html'));
});

app.listen(port, () => {
  console.log(`Example app listening on port ${port}`)
  console.log(`http://localhost:${port}`);
})
