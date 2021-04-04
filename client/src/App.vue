<template>
  <img alt="bees" src="./assets/bees_thumb.png" style="width: 25%" />
  <h3>Poll'em</h3>
  <p>
    A simple, hassle-free and performant polling application written in Haskell.
  </p>
  <requests></requests>
</template>

<script>
import Requests from "./components/Requests.vue";

export default {
  name: "App",
  components: {
    Requests
  },
  data: () => {
    return {
      ready: false
    }
  },
  mounted() {
    this.ready = true;
  }
};
</script>

<style>
#app {
  font-family: Avenir, Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-align: center;
  color: #2c3e50;
  margin-top: 60px;
}
</style>
