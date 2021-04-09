<template>
    <div class="container mb-14 p-8">
        <img
            alt="bees"
            src="../assets/bees_thumb.png"
            style="
                opacity: 60%;
                width: 35%;
                display: block;
                margin-left: auto;
                margin-right: auto;
            "
        />
        <p class="mt-8 font-medium font-sans text-2xl">Poll'em</p>
        <p class="font-sans mb-8">
            A simple & hassle-free poll application
            <span class="font-thin"> with a Haskell backend.</span>
        </p>
        <div>
            <tabs v-model="active">
                <tab title="Your account">
                    <div class="m-4" v-if="loggedIn">
                        <p>Welcome, {{ user.email }}</p>
                        <button
                            v-on:click="logout"
                            class="p-1 bg-red-500 text-white font-semibold rounded-lg shadow-md hover:bg-green-700 focus:ring-opacity-75"
                        >
                            Logout
                        </button>
                    </div>
                    <div class="m-4" v-else>
                        <p>
                            You do not appear to be authenticated on this app.
                            This means you can only view already existing polls.
                            How about you authenticate so that you can start
                            creating polls?
                        </p>
                        <div class="m-8">
                            <div>
                                <input
                                    v-model="user.email"
                                    type="text"
                                    placeholder="email address"
                                    :disabled="user.token_asked"
                                />
                                <button
                                    v-if="!user.token_asked"
                                    v-on:click="askToken"
                                    class="p-1 bg-yellow-500 text-white font-semibold rounded-lg shadow-md hover:bg-green-700 focus:ring-opacity-75"
                                >
                                    Authenticate
                                </button>
                                <button
                                    v-else
                                    class="disabled:opacity-50 p-1 bg-green-500 text-white font-semibold rounded-lg shadow-md"
                                    disabled
                                >
                                    Authenticate
                                </button>
                            </div>
                            <div v-if="user.token_asked" class="mt-4">
                                <input
                                    v-model="user.token"
                                    type="text"
                                    placeholder="paste the token here"
                                />
                                <button
                                    v-if="
                                        user.token.length > 3 &&
                                        !user.token_sent
                                    "
                                    v-on:click="confirmToken"
                                    class="p-1 bg-yellow-500 text-white font-semibold rounded-lg shadow-md hover:bg-green-700 focus:ring-opacity-75"
                                >
                                    Confirm token
                                </button>
                                <button
                                    v-if="user.token_sent"
                                    class="disabled:opacity-50 p-1 bg-green-500 text-white font-semibold rounded-lg shadow-md hover:bg-green-700 focus:ring-opacity-75"
                                    disabled
                                >
                                    Confirm token
                                </button>
                            </div>
                        </div>
                    </div>
                </tab>
                <tab title="Take the poll">
                    <button
                        v-if="AppMode === 'dev' && !takingPoll.startDate"
                        v-on:click="testChart"
                    >
                        Test chart
                    </button>
                    <div v-if="takingPoll.startDate" class="mt-10">
                        <div id="takingPoll" class="flex flex-col">
                            <div>
                                <textarea
                                    id="takingQuestion"
                                    rows="2"
                                    cols="30"
                                    v-bind:value="takingPoll.question"
                                    disabled
                                />
                            </div>
                            <vue-echarts
                                :option="chart.options"
                                :style="chartStyle"
                                ref="chart"
                                v-if="
                                    chart.results.length > 0 &&
                                    takingPoll.visible
                                "
                            />
                            <div>
                                <textarea
                                    id="takingDescription"
                                    rows="3"
                                    cols="45"
                                    v-bind:value="takingPoll.description"
                                    disabled
                                />
                            </div>
                            <div v-for="(a, k) in takingPoll.results" :key="k">
                                <div>
                                    <label>{{ a.text }}</label>
                                    <input
                                        class="ml-4"
                                        type="checkbox"
                                        v-on:change="toggleResults(k)"
                                        v-bind:checked="a.value"
                                    />
                                </div>
                            </div>
                            <div>
                                <label for="takingMultiple"
                                    >Allowed to select multiple answers?</label
                                >
                                <input
                                    class="mx-3 mr-5"
                                    type="checkbox"
                                    id="takingMultiple"
                                    :checked="takingPoll.multiple"
                                    disabled
                                />
                                <label for="takingVisible"
                                    >Results visible during poll?</label
                                >
                                <input
                                    class="mx-3 mr-5"
                                    type="checkbox"
                                    id="takingVisible"
                                    :checked="takingPoll.visible"
                                    disabled
                                />
                            </div>
                            <div class="space-x-3">
                                <label for="takingStartDate"
                                    >Poll started at:</label
                                >
                                <input
                                    type="text"
                                    id="takingStartDate"
                                    v-bind:value="takingPoll.startDate"
                                    disabled
                                />
                                <div v-if="takingPoll.endDate">
                                    <label class="space-y-3" for="takingEndDate"
                                        >(optional) Poll due to close at:</label
                                    >
                                    <input
                                        type="text"
                                        id="takingEndDate"
                                        v-bind:value="takingPoll.endDate"
                                        disabled
                                    />
                                </div>
                            </div>
                        </div>
                        <button
                            v-if="AppMode === 'prod'"
                            v-on:click="takePoll"
                            class="my-5 w-1/3 bg-yellow-500 text-white font-semibold rounded-lg shadow-md hover:bg-green-700 focus:ring-opacity-75"
                        >
                            Take the poll
                        </button>
                        <button
                            v-else
                            v-on:click="testSubmitPoll"
                            class="my-5 w-1/3 bg-yellow-500 text-white font-semibold rounded-lg shadow-md hover:bg-green-700 focus:ring-opacity-75"
                        >
                            Take the poll
                        </button>
                    </div>
                    <div v-else><p>Nothing to render.</p></div>
                </tab>
                <tab title="Create a poll">
                    <div id="creatingPoll" class="flex flex-col">
                        <div>
                            <textarea
                                id="creatingQuestion"
                                rows="2"
                                cols="30"
                                v-model="creatingPoll.question"
                            />
                        </div>
                        <div>
                            <textarea
                                id="creatingDescription"
                                rows="3"
                                cols="45"
                                v-model="creatingPoll.description"
                            />
                        </div>
                        <div>
                            <div
                                class="my-4"
                                v-for="(pa, k) in creatingPoll.answers"
                                :key="'answer' + k"
                            >
                                <label class="mr-4" :for="'creatingAnswer' + k"
                                    >Answer #{{ k + 1 }}</label
                                >
                                <input
                                    type="text"
                                    v-on:input="
                                        creatingPoll.answers[k] =
                                            $event.target.value
                                    "
                                />
                            </div>
                        </div>
                        <div>
                            <button
                                v-on:click="addAnswer"
                                class="p-1 bg-yellow-500 text-white font-semibold rounded-lg shadow-md hover:bg-green-700 focus:ring-opacity-75"
                            >
                                more answers...
                            </button>
                            <button
                                v-on:click="removeAnswer"
                                v-if="creatingPoll.answers.length > 2"
                                class="ml-2 p-1 bg-yellow-500 text-white font-semibold rounded-lg shadow-md hover:bg-green-700 focus:ring-opacity-75"
                            >
                                less
                            </button>
                        </div>
                        <div class="mt-5">
                            <label for="creatingMultiple"
                                >Allow to select multiple answers</label
                            >
                            <input
                                class="mx-3 mr-5"
                                type="checkbox"
                                id="creatingMultiple"
                                v-model="creatingPoll.multiple"
                            />
                            <label for="creatingVisible"
                                >Make results visible during poll</label
                            >
                            <input
                                class="mx-3 mr-5"
                                type="checkbox"
                                id="creatingVisible"
                                v-model="creatingPoll.visible"
                            />
                        </div>
                        <div class="mt-3 space-y-2">
                            <label for="creatingStartDate"
                                >Poll should start at:</label
                            >
                            <datepicker
                                id="creatingStartDate"
                                v-model="creatingPoll.startDate"
                            />
                            <br />
                            <label for="creatingEndDate"
                                >(optional) Poll should end at:</label
                            >
                            <datepicker
                                id="creatingEndDate"
                                v-model="creatingPoll.endDate"
                            />
                        </div>
                    </div>
                    <button
                        v-on:click="createPoll"
                        class="my-5 w-1/3 bg-yellow-500 text-white font-semibold rounded-lg shadow-md hover:bg-green-700 focus:ring-opacity-75"
                    >
                        Create the poll
                    </button>
                </tab>
            </tabs>
        </div>
    </div>
</template>

<script>
import Datepicker from "vue3-datepicker";
import { VueEcharts } from "vue3-echarts";
import FingerprintJS from "@fingerprintjs/fingerprintjs";
import Tabs from "./Tabs";
import Tab from "./Tab";
import { ref } from "vue";

let PollId = null;

const Replies = {
    noLocalStorage:
        "`localStorage API` not supported, this application will not work properly.",
    noCookieSet: "Sorry but you appear to have cleared your cookies",
    loaded: "App loaded successfully.",
    endpoint: "The endpoint ran into an error: ",
};

const Storage = {
    status: true,
    check() {
        if (!localStorage) this.status = false;
        return this.status;
    },
    set(user_storage) {
        localStorage.setItem("pollem-user", JSON.stringify(user_storage));
    },
    get() {
        return JSON.parse(localStorage.getItem("pollem-user"));
    },
    clear() {
        localStorage.removeItem("pollem-user");
    },
};

const Requests = {
    endpoints: {
        dev: "http://localhost:8009",
        prod: "https://pollem-now.herokuapp.com",
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
        if (routes.some((r) => r === route)) return route;
        else return null;
    },
};

export default {
    name: "requests",
    components: {
        Tabs,
        Tab,
        VueEcharts,
        Datepicker,
    },
    data() {
        return {
            AppMode: "prod",//"dev", // "prod"
            creatingPoll: {
                startDate: null,
                endDate: null,
                question: "question...",
                description: "description...",
                multiple: false,
                visible: false,
                answers: ["Answer#1", "Answer#2"],
            },
            takingPoll: {},
            chart: {
                results: [],
                options: {},
            },
            user: {
                token: "",
                hash: "",
                email: "",
                fingerprint: "",
                created: [],
                taken: [],
                token_asked: false,
                token_sent: false,
            },
        };
    },
    setup() {
        // ---------LOADING -------------
        // preparing tabs in relation to a possible GET request
        const uri = window.location.search.substring(1);
        PollId = new URLSearchParams(uri).get("poll_id");
        const active = PollId === null ? ref(0) : ref(1);
        return { active };
    },
    mounted() {
        // loading fingerprinting library
        FingerprintJS.load()
            .then((fp) => fp.get())
            .then((result) => {
                // loaded, binding fingerprint
                if (!Storage.check()) this.$toast.error(Replies.noLocalStorage);
                else {
                    // restoring storage
                    this.user =
                        Storage.get() === null ? this.user : Storage.get();
                    // saving fingerprint to component's data
                    this.user.fingerprint = result.visitorId;
                }
                this.creatingPoll.startDate = new Date();
                // ------------- END LOADING ------------
                // exiting loading if we're are not GET-ing any poll
                if (PollId === null) {
                    this.$toast.success(Replies.loaded);
                    return;
                    // otherwise fetching poll passed as parameter
                }
                return (
                    fetch(
                        Requests.endpoints[this.AppMode] +
                            "/" +
                            PollId.toString()
                    )
                        // error, bubbling up to user
                        .catch((err) => this.$toast.error(err))
                        // parsing result
                        .then((res) => res.json())
                        // binding results to component's data, displaying, bubbling up confirmation
                        .then((res) => {
                            if (!res.resp_get_poll) {
                                this.$toast.error(
                                    "Either the poll was not received or could not be decoded, aborting. Unable to carry on."
                                );
                                return;
                            }
                            const poll = JSON.parse(res.resp_get_poll);
                            this.takingPoll = poll;
                            this.takingPoll.results = poll.answers.map((a) => ({
                                text: a,
                                value: false,
                            }));
                            if (res.resp_get_poll_results) {
                                this.chart.results = res.resp_get_poll_results.map(
                                    (d) => parseInt(d)
                                );
                                this.setChartOptions();
                            }
                            this.$toast.success(
                                Replies.loaded +
                                    " Here is your poll. (" +
                                    res.resp_get_poll_msg +
                                    ")"
                            );
                        })
                );
            });
    },
    computed: {
        loggedIn() {
            return [
                this.user.token,
                this.user.hash,
                this.user.email,
                this.user.fingerprint,
            ].every((i) => i !== "");
        },
        chartStyle() {
            const x = this.takingPoll.answers.length;
            return x === 0
                ? null
                : "width: 900px; height: " + (75 * x).toString() + "px";
        },
    },
    methods: {
        testUser() {
            this.user.email = "garbo email";
            this.user.hash = "garbo hash";
            this.user.fingerprint = "garbo fingerprint";
            this.user.token = "garbo token";
        },
        testChart() {
            const mockPoll = {
                startDate: new Date(),
                question: "How to make an omelette without breaking any egg?",
                description: "Some description",
                multiple: false,
                visible: true,
                answers: [
                    "AnswerA",
                    "AnswerB",
                    "AnswerC",
                    "AnswerD",
                    "AnswerE",
                    "AnswerF",
                    "AnswerG",
                ],
            };
            const payload = {
                resp_get_poll_results: [3, 5, 9, 8, 7, 8, 1],
                resp_get_poll: JSON.stringify(mockPoll),
                resp_get_poll_msg: "Ok fine",
            };
            return Promise.resolve(JSON.stringify(payload))
                .catch((err) => this.$toast.error(err))
                .then((res) => {
                    const p = JSON.parse(res);
                    this.takingPoll = JSON.parse(p.resp_get_poll);
                    this.takingPoll.results = this.takingPoll.answers.map(
                        (a) => ({ text: a, value: false })
                    );
                    if (p.resp_get_poll_results) {
                        this.chart.results = p.resp_get_poll_results.map((d) =>
                            parseInt(d)
                        );
                        this.setChartOptions();
                    }
                    this.$toast.success(p.resp_get_poll_msg);
                });
        },
        testSubmitPoll() {
            const payload = {
                take_fingerprint: this.user.fingerprint,
                take_hash: this.user.hash,
                take_token: this.user.token,
                take_results: this.takingPoll.answers.map(
                    (r) =>
                        this.takingPoll.results.find((x) => x.text === r).value
                ),
                take_pollid: PollId,
            };
            console.log(JSON.stringify(payload));
        },
        setChartOptions() {
            this.chart.options = {
                yAxis: {
                    type: "category",
                    data: [].concat(this.takingPoll.answers).reverse(), // apparently echart applies some weird sorting
                },
                xAxis: {
                    type: "value",
                },
                series: [
                    {
                        data: this.chart.results,
                        type: "bar",
                        showBackground: false,
                        backgroundStyle: {
                            color: "rgba(0, 177, 124, 1)",
                        },
                    },
                ],
            };
        },
        toggleResults(k) {
            this.takingPoll.results[k].value = !this.takingPoll.results[k]
                .value;
            if (!this.takingPoll.multiple)
                this.takingPoll.results.forEach(
                    (item, index) =>
                        (item.value = index === k ? item.value : false)
                );
        },
        addAnswer() {
            const i = this.creatingPoll.answers.length + 1;
            this.creatingPoll.answers.push("Answer#" + i.toString());
        },
        removeAnswer() {
            this.creatingPoll.answers.pop();
        },
        // ----------- REQUESTS --------------
        makeReq(route, payload) {
            const e = Requests.endpoints[this.AppMode]; // Requests.endpoints["prod"]
            const r = Requests.tryRoute(route);
            if (r === null) {
                this.$toast.error("Bad endpoint! Request aborted.")
                return;
            } 
            const url = e + r;
            if (route === "/get")
                return fetch(url)
                    .catch((err) => this.$toast.error(err))
                    .then((res) => res.json());
            else {
                const config = {
                    headers: { "Content-Type": "application/json", "Accept": "application/json" },
                    method: "POST",
                    body: JSON.stringify(payload),
                };
                return fetch(url, config)
                    .catch((err) => this.$toast.error(err))
                    .then((res) => res.json());
            }
        },
        askToken() {
            this.user.token_asked = true;
            const payload = {
                ask_email: this.user.email,
            };
            return this.makeReq("/ask_token", payload).then((res) => {
                this.$toast.success(res.resp_ask_token, {
                    duration: 15000,
                    dismissible: false,
                });
            });
        },
        confirmToken() {
            this.user.token_sent = true;
            const payload = {
                req_confirm_token: this.user.token,
                req_confirm_fingerprint: this.user.fingerprint,
                req_confirm_email: this.user.email,
            };
            return this.makeReq("/confirm_token", payload).then((res) => {
                if (res.resp_confirm_token && res.resp_confirm_hash) {
                    this.user.token = res.resp_confirm_token;
                    this.user.hash = res.resp_confirm_hash;
                    this.$toast.success(res.resp_confirm_msg);
                } else {
                    this.$toast.warning(res.resp_confirm_msg);
                }
            });
        },
        createPoll() {
            const payload = {
                create_hash: this.user.hash,
                create_token: this.user.token,
                create_recipe: JSON.stringify(this.creatingPoll),
                create_startDate: this.creatingPoll.startDate,
            };
            if (this.creatingPoll.endDate !== null)
                payload.req_create_endDate = this.creatingPoll.endDate;
            return this.makeReq("/create", payload).then((res) =>
                this.$toast.success(res.resp_create_msg)
            );
        },
        closePoll() {
            const payload = {
                close_hash: this.user.hash,
                close_token: this.user.token,
                close_pollid: PollId,
            };
            return this.makeReq("/close", payload).then((res) =>
                this.$toast.success(res.resp_close_msg)
            );
        },
        takePoll() {
            const payload = {
                take_fingerprint: this.user.fingerprint,
                take_hash: this.user.hash,
                take_token: this.user.token,
                take_results: this.takingPoll.answers.map(
                    (r) =>
                        this.takingPoll.results.find((x) => x.text === r).value
                ),
                take_pollid: PollId,
            };
            return this.makeReq("/take", payload).then((res) =>
                this.$toast.success(res.resp_take_msg)
            );
        },
        logout() {
            this.user = {
                token: "",
                hash: "",
                email: "",
                fingerprint: "",
                created: [],
                taken: [],
                token_asked: false,
                token_sent: false,
            };
            Storage.clear();
        },
    },
    watch: {
        user: {
            deep: true,
            handler(user) {
                if (
                    [
                        this.user.token,
                        this.user.hash,
                        this.user.email,
                        this.user.fingerprint,
                    ].every((i) => i !== "")
                ) {
                    Storage.set(user);
                }
            },
        },
    },
};
</script>
<style scoped>
body {
    padding-top: 40px;
}
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
