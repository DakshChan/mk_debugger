import {useEffect, useState} from "react";
import axios from "axios";
import { useForm } from "react-hook-form";
import "./DebuggerPanel.css";

export default function DebuggerPanel({setDebug}) {
  const {register, handleSubmit, errors} = useForm();

  const executeQuery = (query) => {
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
      <form className={"panelForm"} onSubmit={handleSubmit(executeQuery)}>
        <button>Run</button>
        <label>Solutions</label>
        <input type={"number"} {...register("solutionNum")} defaultValue={-1}/>
        <label>Samples</label>
        <input type={"number"} {...register("sampleNum")} defaultValue={-1}/>
        <label>Query</label>
        <input type={"text"} {...register("query")} defaultValue={''}/>
      </form>
    </>
  );
}


