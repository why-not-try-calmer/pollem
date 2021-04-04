<template>
  <vue-echarts :option="option" style="height: 500px" ref="chart" />
</template>
<script>
import { VueEcharts } from "vue3-echarts";
export default {
  name: "visualizer",
  components: {
    VueEcharts
  },
  data: () => {
    return {
      option: {
        xAxis: {
          type: "category",
          data: ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"],
        },
        yAxis: {
          type: "value",
        },
        series: [
          {
            data: [120, 200, 150, 80, 70, 110, 130],
            type: "bar",
            showBackground: true,
            backgroundStyle: {
              color: "rgba(180, 180, 180, 0.2)",
            },
          },
        ],
      },
    };
  },
};
</script>
