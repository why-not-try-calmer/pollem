<template>
  <div>
    <pure-vue-chart :points="[3, 5, 2, 5, 4]" :width="400" :height="200" />
  </div>
</template>

<script>
import PureVueChart from "pure-vue-chart";

export default {
  name: "visualizer",
  components: PureVueChart,
  data: () => {
    return {};
  },
};
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
h3 {
  margin: 40px 0 0;
}
ul {
  list-style-type: none;
  padding: 0;
}
li {
  display: inline-block;
  margin: 0 10px;
}
a {
  color: #42b983;
}
</style>
