<template>
  <div>
    <p>Your fingerprint: {{ user.fingerprint }}</p>
    <button v-on:click="create_poll">Create a poll</button>
    <p>
      <vue-echarts
        v-if="renderChart"
        :option="chartOptions"
        style="margin: 0 auto; height: 300px; width: 900px"
        ref="chart"
      />
    </p>
  </div>
</template>

<script>
import { VueEcharts } from "vue3-echarts";
import FingerprintJS from "@fingerprintjs/fingerprintjs";

const Errors = {
  taken: "Sorry but you appear to have taken the survey already.",
  noLocalStorage:
    "`localStorage API` not supported, this application will not work properly.",
  noCookieSet: "Sorry but you appear to have cleared your cookies",
};

const Storage = {
  checks(fingerprint) {
    if (!localStorage) {
      throw new Error(Errors.noLocalStorage);
    }
    let user_storage = this.get();
    if (!user_storage) {
      user_storage = {
        fingerprint: fingerprint,
        hash: null,
        created: [],
        taken: [],
      };
    }
    this.set(user_storage);
    return Promise.resolve("App could initialize properly.");
  },
  set(user_storage) {
    localStorage.setItem("pollem-user", JSON.stringify(user_storage));
  },
  get() {
    return JSON.parse(localStorage.getItem("pollem-user"));
  },
};

const Requests = {
  endpoint: "http://localhost:8080",
  headers: { Accept: "application/json", "Content-type": "application/json" },
  preReq(payload) {
    return {
      method: "POST",
      headers: this.headers,
      body: JSON.stringify(payload),
    };
  },
  tryRoute(route) {
    const routes = [
      "/ask_token",
      "/confirm_token",
      "/create",
      "/close",
      "/get",
      "/take",
    ];
    if (routes.includes(route)) return route;
    else throw new Error("Wrong route: " + route);
  },
};

export default {
  name: "requests",
  components: {
    VueEcharts,
  },
  data: () => {
    return {
      poll: {
        startDate: null,
        endDate: "",
        question: "",
        description: "",
        multiple: null,
        visible: null,
        answers: [],
        id: null,
      },
      user: {
        token: "",
        hash: "",
        email: "",
        fingerprint: "",
      },
      renderChart: false
    };
  },
  mounted() {
    FingerprintJS.load()
      .then((fp) => fp.get())
      .then((result) => {
        const fingerprint = result.visitorId;
        this.user.fingerprint = fingerprint;
        return Storage.checks(fingerprint)
          .catch((err) => this.$toast.error(err))
          .then((res) => {
            this.$toast.warning("Loading results...")
            setTimeout(() => { 
              this.renderChart = true
              this.$toast.info(res)
            }, 2000 )
          })
      });
  },
  computed: {
    chartOptions() {
      return {
        yAxis: {
          type: "category",
          data: ["Question 1", "Question 2", "Question 3", "Question 4", "Question 5"],
        },
        xAxis: {
          type: "value",
        },
        series: [
          {
            data: [120, 200, 150, 80, 70, 110, 130],
            type: "bar",
            showBackground: false,
            backgroundStyle: {
              color: "rgba(180, 180, 180, 0.2)",
            },
          },
        ],
      }
    }
  },
  methods: {
    buildChart() {
      this.option = {
        yAxis: {
          type: "category",
          data: ["Question 1", "Question 2", "Question 3", "Question 4", "Question 5"],
        },
        xAxis: {
          type: "value",
        },
        series: [
          {
            data: [120, 200, 150, 80, 70, 110, 130],
            type: "bar",
            showBackground: false,
            backgroundStyle: {
              color: "rgba(180, 180, 180, 0.2)",
            },
          },
        ],
      }
    },
    makeReq(route, payload) {
      return fetch(
        Requests.endpoint + Requests.tryRoute(route),
        Requests.preReq(payload)
      )
        .then((res) => this.$toast.success(JSON.parse(res)))
        .catch((err) => this.$toast.error(err));
    },
    ask_token() {
      const payload = {
        ask_email: this.user.email,
      };
      return this.makeReq("/ask", payload);
    },
    confirm_token() {
      const payload = {
        req_confirm_token: this.user.token,
        req_confirm_fingerprint: this.user.fingerprint,
        req_confirm_email: this.user.email,
      };
      return this.makeReq("/confirm_token", payload);
    },
    create_poll() {
      const payload = {
        req_create_hash: this.user.hash,
        req_create_token: this.user.token,
        req_create_recipe: JSON.stringify(this.poll),
        req_create_startDate: this.poll.startDate,
        req_create_endDate: this.poll.endDate,
      };
      return this.makeReq("/create", payload);
    },
    close_poll() {
      const payload = {
        close_hash: this.user.hash,
        close_token: this.user.token,
        close_pollid: this.poll.id,
      };
      return this.makeReq("/close", payload);
    },
    take_poll() {
      const payload = {
        take_hash: this.user.hashs,
        take_token: this.user.token,
        take_fingerprint: this.user.fingerprint,
        take_pollid: this.poll.id,
        take_answers: this.poll.answers,
      };
      return this.makeReq("/take", payload);
    },
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
