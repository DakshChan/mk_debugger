import axios from "axios";
import {useState} from "react";

export default function UploadCode({setCode, setDebug, fileName, setFileName}) {
  function handleChange(event) {

    const file = event.target.files[0];
    console.log(file);
    const url = 'http://localhost:3000/code';
    const formData = new FormData();
    formData.append('file', file);
    formData.append('fileName', file.name);
    const config = {
      headers: {
        'content-type': 'multipart/form-data',
      },
    };
    axios.post(url, formData, config).then((response) => {
      console.log(response.status);
      setCode(response.data);
      setDebug(undefined);
      setFileName(file.name);
    }).catch((error) => {
      console.log(error);
    });
    event.target.value = '';
    event.target.placeholder = 'Upload code';
  }

  return (
    <>
      <label htmlFor="file_upload">
        <button style={{pointerEvents: "none"}}>Choose file</button>
        <span style={{fontSize:"small", marginInline:"1ch"}}>
          {fileName !== "" ? fileName : "No file chosen"}
        </span>
      </label>
      <input
        style={{opacity: 0, width:0, height:0}}
        id="file_upload" type="file" onChange={handleChange}
      />
    </>
  )
}
