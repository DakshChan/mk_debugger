import axios from "axios";
import {useEffect, useState} from "react";

export default function UploadCode({setCode}) {
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
    });
  }

  return (
    <>
      <input type="file" onChange={handleChange}/>
    </>
  )
}
