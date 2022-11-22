import axios from "axios";
import { useForm } from "react-hook-form";
import {useEffect, useState} from "react";
import { useLocalStorage } from '@rehooks/local-storage';
import "./DebuggerPanel.css";
import RunningIndicator from "./RunnningIndicator";

export default function RunResume({setDebug, fileName}) {
  const {register, handleSubmit} = useForm();
  const [queryCache, setQueryCache] = useLocalStorage('queryCache', {});
  const [running, setRunning] = useState(false);
  const [timeInfo, setTimeInfo] = useState(null);

  const executeQuery = (query) => {
    console.log(query);
    if (fileName !== "") setQueryCache({...queryCache, [fileName]:query});
    let solutions = parseInt(query.solutions);
    solutions = solutions > -1 ? solutions : "#f";
    let samples = parseInt(query.samples);
    samples = samples > -1 ? samples : "#f";
    let steps = parseInt(query.steps);
    steps = steps > -1 ? steps : "#f";
    query.solutions = solutions.toString();
    query.samples = samples.toString();
    query.steps = steps.toString();
    console.log(query);
    const url = 'http://localhost:3000/debug';
    setRunning(true);
    axios.postForm(url, query).then((response) => {
      console.log(response.data);
      setDebug(response.data);
      setTimeInfo(response.data.time);
      setRunning(false);
    }).catch((error) => {
      console.log(error);
      setRunning(false);
    });
  }

  const killProcess = (e) => {
    e.preventDefault();
    const url = 'http://localhost:3000/kill';
    axios.post(url).then((response) => {
      console.log(response.status);
      setRunning(false);
    }).catch((error) => {
      console.log(error)
      setRunning(false);
    });
  }

  return (
    <>
      <form className={"panelForm"} key={fileName}>
        <button onClick={handleSubmit((d) => executeQuery({...d, "command": "run"}))}>Run</button>
        <button onClick={handleSubmit((d) => executeQuery({...d, "command": "resume"}))}>Resume</button>
        <button onClick={killProcess}>Kill</button>
      </form>
      <RunningIndicator running={running} timeInfo={timeInfo}/>
    </>
  );
}


