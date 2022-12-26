import CodeContainer from "./CodeContainer";
import UploadCode from "./UploadCode";
import PointInfoPanel from "./PointInfoPanel";
import { useEffect, useState } from "react";
import './App.css';
import SolutionInfoPanel from "./SolutionInfoPanel";
import RejectionInfoPanel from "./RejectionInfoPanel";
import RunResume from "./RunResume";
import { Select, useToast } from "@chakra-ui/react";
import {
  Alert,
  AlertIcon,
  AlertTitle,
} from '@chakra-ui/react'

import io from 'socket.io-client';
import QueryPanel from "./QueryPanel";
const socket = io();

function App() {
  const [code, setCode] = useState("");
  const [debug, setDebug] = useState(undefined);
  const [codeHighlight, setCodeHighlight] = useState({"info": "failures", "style": "color"});
  const [pointDebug, setPointDebug] = useState(undefined);
  const [fileName, setFileName] = useState("");
  const [running, setRunning] = useState(false);
  const [queries, setQueries] = useState({});
  const [serverConnected, setServerConnected] = useState(true);

  const toast = useToast();

  useEffect(() => {
    socket.on('connect', () => {
      console.log("Connected to server");
      setServerConnected(true);
      toast({
        position: "bottom-left",
        title: "Connected to server",
        status: "success",
        duration: 3000,
        isClosable: true
      })
    });

    socket.on('connect_error', () => {
      console.log("Connection to server failed");
      setServerConnected(false);
      toast({
        position: "bottom-left",
        title: "Connection to server failed",
        status: "error",
        duration: 3000,
        isClosable: true
      })
    });

    socket.on('disconnect', () => {
      console.log("Disconnected from server");
      setServerConnected(false);
    });

    socket.on('exit', (code, signal) => {
      console.log("Racket process exited with code", code, "and signal", signal);
      setRunning(false);
      toast({
        position: "bottom-left",
        title: "Racket process exited",
        description: `Code: ${code}, Signal: ${signal}`,
        status: "error",
        isClosable: true
      });
    });

    return () => {
      socket.off('connect');
      socket.off('disconnect');
      socket.off('exit');
    };
  }, []);

  const sendKill = () => {
    socket.emit('kill', (data) => {
      console.log(data);
      setRunning(false);
      toast({
        position: "bottom-left",
        title: "Racket process killed",
        status: "success",
        isClosable: true
      });
    });
  };

  const sendCode = (file, fileName) => {
    socket.emit('code', file, fileName, (data) => {
      console.log(data);
      setCode((new TextDecoder("utf-8")).decode(data.file));
      setDebug(undefined);
      setFileName(data.fileName);
      setRunning(false);
      toast({
        position: "bottom-left",
        title: "Code uploaded",
        status: "success",
        isClosable: true
      });
    });
  };

  const sendQuery = (q) => {
    setRunning(true);
    let query = {...q, ...queries};
    console.log(query);
    socket.emit('query', query, (data) => {
      console.log(data);
      setDebug(data.data);
      setRunning(false);
      toast({
        position: "bottom-left",
        title: data.message,
        status: (data.status === 200) ? "success" : "error",
        ...((data.status === 500) ? {description: data.body} : {}),
        isClosable: true
      });
    });
  }

  return (
    <div style={{display: "flex", flexDirection: "column", gap: "0.2em"}}>
      {(serverConnected) ? <></> :
        <Alert status='error'>
          <AlertIcon />
          <AlertTitle>Server not connected!</AlertTitle>
        </Alert>
      }
      <div style={{display: "flex", gap: "0.2em"}}>
        <UploadCode sendCode={sendCode} fileName={fileName}/>
        <RunResume running={running} sendQuery={sendQuery} sendKill={sendKill} debug={debug}/>
      </div>
      <div style={{display: "flex", gap: "0.2em"}}>
        <div style={{display: "flex", flexDirection: "column", gap:"0.2em"}}>
          <QueryPanel setQueries={setQueries}/>
          <div style={{display: "flex", alignItems:"baseline", gap: "1ch"}}>
            <p>Highlighting</p>
            <Select width={"unset"} size={"sm"} defaultValue={"failures"} onChange={(event) => setCodeHighlight({...codeHighlight, "info": event.target.value})}>
              <option value={"encounters"}>Encounters</option>
              <option value={"failures"}>Failures</option>
              <option value={"failRatio"}>Failure Ratio</option>
              <option value={"successes"}>Successes</option>
              <option value={"successRatio"}>Success Ratio</option>
            </Select>
          </div>
          <CodeContainer code={code} debug={debug} codeHighlight={codeHighlight} pointDebug={pointDebug} setPointDebug={setPointDebug}/>
        </div>
        <div style={{width: "-webkit-fill-available"}}>
          <PointInfoPanel pointDebug={pointDebug} code={code}/>
          <SolutionInfoPanel debug={debug} code={code}/>
          <RejectionInfoPanel debug={debug} code={code}/>
        </div>
      </div>
    </div>
  );
}

export default App;
