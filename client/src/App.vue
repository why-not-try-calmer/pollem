<template>
  <requests></requests>
</template>

<script>
import Requests from "./components/Requests.vue";

export default {
  name: "App",
  components: {
    Requests,
  },
};
</script>

<style>
#app {
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-align: center;
  color: #2c3e50;
  background-color: hsl(39, 58%, 95%);
  margin-top: 60px;
}
</style>
