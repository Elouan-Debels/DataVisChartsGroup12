<script>
  import Chart from "./lib/Chart.svelte";
</script>

<main class="main">
  <div class="container">
      <Chart />
  </div>

</main>

<style>
  .main {
    background-color: #3c5b6f;
    max-width: 100%;
    min-height: 100vh;

    margin: auto;

    border: 4px solid green;
  }
  .container{
    margin-top: 4em;
    display: flex;
    justify-content: center;
  }
</style>
