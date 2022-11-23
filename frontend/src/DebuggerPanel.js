import axios from "axios";
import { useForm } from "react-hook-form";
import { useEffect } from "react";
import { useLocalStorage } from '@rehooks/local-storage';
import "./DebuggerPanel.css";

export default function DebuggerPanel({setDebug, fileName}) {
  const {register, handleSubmit} = useForm();
  const [queryCache, setQueryCache] = useLocalStorage('queryCache', {});

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
    setDebug(undefined);
    axios.postForm(url, query).then((response) => {
      console.log(response.data);
      setDebug(response.data);
    }).catch((error) => {
      console.log(error);
    });
  }

  const killProcess = (e) => {
    e.preventDefault();
    const url = 'http://localhost:3000/kill';
    setDebug(undefined);
    axios.post(url).then((response) => {
      console.log(response.status);
    }).catch((error) => {
      console.log(error);
    });
  }

  return (
    <>
      <form className={"panelForm"} key={fileName}>
        <button onClick={handleSubmit((d) => executeQuery({...d, "command": "run"}))}>Run</button>
        <button onClick={handleSubmit((d) => executeQuery({...d, "command": "resume"}))}>Resume</button>
        <button onClick={killProcess}>Kill</button>
        <label>Solutions</label>
        <input key={"solutions"+fileName} type={"number"} {...register("solutions")} defaultValue={queryCache[fileName]?.solutions ?? -1}/>
        <label>Samples</label>
        <input key={"samples"+fileName} type={"number"} {...register("samples")} defaultValue={queryCache[fileName]?.samples ?? 0}/>
        <label>Steps</label>
        <input key={"steps"+fileName} type={"number"} {...register("steps")} defaultValue={queryCache[fileName]?.steps ?? -1}/>
        <label>Query</label>
        <input key={"query"+fileName} type={"text"} {...register("query")} defaultValue={queryCache[fileName]?.query ?? ""}/>
      </form>
    </>
  );
}


