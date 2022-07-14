import {useEffect, useState} from "react";
import axios from "axios";
import { useForm } from "react-hook-form";
import "./DebuggerPanel.css";

export default function DebuggerPanel({setDebug}) {
  const {register, handleSubmit} = useForm();

  const executeQuery = (query) => {
    console.log(query);
    const url = 'http://localhost:3000/debug';
    axios.postForm(url, query).then((response) => {
      console.log(response.data);
      setDebug(response.data);
    }).catch((error) => {
      console.log(error);
    });
  }

  return (
    <>
      <form className={"panelForm"}>
        <button onClick={handleSubmit((d) => executeQuery({...d, "runResume": "run"}))}>Run</button>
        <button onClick={handleSubmit((d) => executeQuery({...d, "runResume": "resume"}))}>Resume</button>
        <label>Solutions</label>
        <input type={"number"} {...register("solutionNum")} defaultValue={-1}/>
        <label>Samples</label>
        <input type={"number"} {...register("sampleNum")} defaultValue={-1}/>
        <label>Steps</label>
        <input type={"number"} {...register("stepNum")} defaultValue={-1}/>
        <label>Query</label>
        <input type={"text"} {...register("query")} defaultValue={''}/>
      </form>
    </>
  );
}


