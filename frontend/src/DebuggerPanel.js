import axios from "axios";
import { useForm } from "react-hook-form";
import "./DebuggerPanel.css";

export default function DebuggerPanel({setDebug}) {
  const {register, handleSubmit} = useForm();

  const executeQuery = (query) => {
    console.log(query);
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
    axios.post(url).then((response) => {
      console.log(response.status);
    }).catch((error) => {
      console.log(error);
    });
  }

  return (
    <>
      <form className={"panelForm"}>
        <button onClick={handleSubmit((d) => executeQuery({...d, "command": "run"}))}>Run</button>
        <button onClick={handleSubmit((d) => executeQuery({...d, "command": "resume"}))}>Resume</button>
        <button onClick={killProcess}>Kill</button>
        <label>Solutions</label>
        <input type={"number"} {...register("solutions")} defaultValue={-1}/>
        <label>Samples</label>
        <input type={"number"} {...register("samples")} defaultValue={0}/>
        <label>Steps</label>
        <input type={"number"} {...register("steps")} defaultValue={-1}/>
        <label>Query</label>
        <input type={"text"} {...register("query")} defaultValue={''}/>
      </form>
    </>
  );
}


