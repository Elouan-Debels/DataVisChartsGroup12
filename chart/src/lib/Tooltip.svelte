<script>
  export let x;
  export let y;
  export let data;
  export let key;
  export let selected = false;

  function capitalize(s) {
    return s[0].toUpperCase() + s.slice(1);
  }
</script>

<div
  class="tooltip"
  style="position: absolute;
    top: {y -15}px;
    left: {x+5}px;
    opacity: {selected ? "1" : "0"}"
>
 {data}-{capitalize(key.split(/(?=[A-Z])/).join(" "))}
</div>

<style>
  .tooltip {
    background: #fafafa;
    box-shadow: rgba(0, 0, 0, 0.15) 2px 3px 8px;
    padding: 8px 6px;
    border-radius: 3px;
    pointer-events: none;
    font-size: 0.875em;
  }
  
</style>
