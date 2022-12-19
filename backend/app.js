const express = require('express');
const path = require('path');
const { resolve } = require('path');
const cors = require('cors');
const formData = require('express-form-data');
const { spawn, spawnSync, exec } = require('child_process');
const fs = require('fs');
const uuid = require('uuid');
const cookieParser = require("cookie-parser");
const sessions = require('express-session');
const socket = require("socket.io");

const app = express();
const port = process.env.PORT || 3000;


app.use(formData.parse());
app.use(express.static(path.join(__dirname, 'build')))
app.use(sessions({secret: "gamer"}))

// cors only being used when running react server, remove after react build
app.use(cors({
  origin: '*'
}));

let racket = null;
let racketBuf = null;
let codeContent = null;
let userUUID = null;

app.post('/kill', (req, res) => {
  racket?.kill();
  racket = null;
  racketBuf = null;
  res.status(200).send('killed');
});

app.post('/code', (req, res) => {
  //TODO: potentially reformat before sending back to client
  let userCode = req.files.file.path;
  console.log(userCode);
  racket?.kill();
  racketBuf = null;

  codeContent = fs.readFileSync(userCode, 'utf8').toString();

  console.log(resolve("./debugger/mk-fo.rkt").replaceAll("\\", "\\\\"));
  userUUID = uuid.v4();
  if (!fs.existsSync(`./tmp/${userUUID}`)) {
    fs.mkdirSync(`./tmp/${userUUID}`);
  }

  console.log(userUUID);
  fs.writeFileSync(`./tmp/${userUUID}/user-code.rkt`, codeContent);
  userUploadedCode = `./tmp/${userUUID}/user-code.rkt`;

  res.sendFile(userCode);
});

app.post('/debug', (req, res) => {
  console.log(JSON.stringify(req.body));

  if (userUUID === null) {
    res.status(400).send("No code uploaded");
    return;
  }

  racketBuf = "";
  racket?.stdout?.removeAllListeners();
  racket?.stderr?.removeAllListeners();

  if (req.body.command === "run") { // run
    racket?.kill();
    // racket = spawn("racket", [userRepl]);
    // racket = spawn("bash", ["-c", `docker run --rm -it $(docker build -q --build-arg userRepl=${userUUID} -f racketDockerfile .)`], {detached: true});
    racket = spawn("bash", ["-c", `docker run --rm -i $(docker build --rm -q --build-arg userRepl=${userUUID} -f racketDockerfile .); wait -n`]);
    //let d = spawnSync(`docker build --build-arg userRepl=${userUUID} -f racketDockerfile .`);
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
      console.log(JSON.stringify(resultJSON));
      resultJSON["program-points"] = resultJSON["program-points"].filter(x => x.syntax.source !== false)
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

    racket?.stdout.removeAllListeners();
    racket?.stderr.removeAllListeners();
    racket?.removeAllListeners(); // so that the exit event doesn't fire when we kill it
    racket?.kill();
    racket = null;
  });

  racket.on('exit', (code, signal) => {
    try {
      res.status(400).send("racket process exited");
    } catch (e) {

    }
  })
});

app.get('(/*)?', (req, res) => {
  res.sendFile(path.join(__dirname, 'build', 'index.html'));
});

const server = app.listen(port, () => {
  if (!fs.existsSync("./tmp")){
    fs.mkdirSync("./tmp");
  }
  // process.env["DOCKER_HOST"] = "unix:///var/run/docker.sock";
  // console.log(process.env)
  console.log(`Example app listening on port ${port}`)
  console.log(`http://localhost:${port}`);
})

const io = socket(server, {cookie: false});

io.on("connection", (socket) => {
  console.log("made socket connection", socket.id);
  socket.mk_profiler = {};

  socket.on("disconnect", () => {
    console.log("user disconnected");
  });
});