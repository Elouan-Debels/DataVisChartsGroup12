<script>
    export let x = 0;
    export let y = 0;
    export let selected =false;
  </script>
  
  <g>
    <circle class={selected ? 'active-point' : "point"} cx={x} cy={y} r="4" stroke="white" stroke-width="1"/>
  </g>
  
  <style>
    .point {
      fill: #000;
    }
    .active-point{
      fill: #DED0B6;
    }
  </style>