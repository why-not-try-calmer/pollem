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
                            @click="logout"
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
                                    @click="askToken"
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
                                    @click="confirmToken"
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
                        @click="testChart"
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
                                    :value="takingPoll.question"
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
                                    :value="takingPoll.description"
                                    disabled
                                />
                            </div>
                            <div v-for="(a, k) in takingPoll.results" :key="k">
                                <div>
                                    <label>{{ a.text }}</label>
                                    <input
                                        class="ml-4"
                                        type="checkbox"
                                        @change="toggleResults(k)"
                                        :checked="a.value"
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
                                    :value="takingPoll.startDate"
                                    disabled
                                />
                                <div v-if="takingPoll.endDate">
                                    <label class="space-y-3" for="takingEndDate"
                                        >(optional) Poll due to close at:</label
                                    >
                                    <input
                                        type="text"
                                        id="takingEndDate"
                                        :value="takingPoll.endDate"
                                        disabled
                                    />
                                </div>
                            </div>
                        </div>
                        <button
                            v-if="AppMode === 'prod'"
                            @click="takePoll"
                            class="my-5 w-1/3 bg-yellow-500 text-white font-semibold rounded-lg shadow-md hover:bg-green-700 focus:ring-opacity-75"
                        >
                            Take the poll
                        </button>
                        <button
                            v-else
                            @click="testSubmitPoll"
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
                                    @input="
                                        creatingPoll.answers[k] =
                                            $event.target.value
                                    "
                                />
                            </div>
                        </div>
                        <div>
                            <button
                                @click="addAnswer"
                                class="p-1 bg-yellow-500 text-white font-semibold rounded-lg shadow-md hover:bg-green-700 focus:ring-opacity-75"
                            >
                                more answers...
                            </button>
                            <button
                                @click="removeAnswer"
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
                        @click="createPoll"
                        class="my-5 w-1/3 bg-yellow-500 text-white font-semibold rounded-lg shadow-md hover:bg-green-700 focus:ring-opacity-75"
                    >
                        Create the poll
                    </button>
                </tab>
                <tab title="My Polls">
                    <div v-if="mypolls">
                        <p v-if="user.created.length > 0" class="font-bold">
                            Created
                        </p>
                        <div
                            v-for="(t, k) in user.created"
                            :key="k"
                            class="grid grid-cols-3 gap-1"
                        >
                            <input :value="t.question" disabled />
                            <input :value="t.startDate" disabled />
                            <input
                                v-if="t.endDate"
                                :value="t.endDate"
                                disabled
                            />
                            <a :href="t.link">Go to poll</a>
                        </div>
                        <p v-if="user.taken.length > 0" class="font-bold">
                            Taken
                        </p>
                        <div
                            v-for="(t, k) in user.taken"
                            :key="k"
                            class="grid grid-cols-3 gap-1"
                        >
                            <input :value="t.question" disabled />
                            <input :value="t.startDate" disabled />
                            <input
                                v-if="t.endDate"
                                :value="t.endDate"
                                disabled
                            />
                            <a :href="t.link">Go to poll</a>
                        </div>
                    </div>
                    <div v-else>
                        <p>
                            You've taken or created polls you think should be
                            displayed here? You can retrieve your entire
                            history. Please be considerate in your use of this
                            feature, as the database is not optimized for this
                            (expect serious debounce). Also your last created
                            poll might be missing if you created it very
                            recently.
                        </p>
                        <button
                            @click="restoreHistory"
                            class="p-1 bg-yellow-500 text-white font-semibold rounded-lg shadow-md hover:bg-green-700 focus:ring-opacity-75"
                        >
                            Retrieve entire history
                        </button>
                    </div>
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
    routes: {
        post: {
            ask_token: ["ask_email"],
            close: ["close_hash", "close_token", "close_pollid"],
            confirm_token: [
                "confirm_token",
                "confirm_fingerprint",
                "confirm_email",
            ],
            create: [
                "poll_startDate",
                "poll_question",
                "poll_description",
                "poll_visible",
                "poll_multiple",
                "poll_answers",
            ],
            myhistory: ["myhistory_hash", "myhistory_token"],
            take: [
                "take_fingerprint",
                "take_hash",
                "take_token",
                "take_results",
            ],
        },
        get: ["get", "warmup"],
    },
    tryRoute(method, route) {
        if (Object.keys(this.routes[method]).some((r) => r === route))
            return "/" + route;
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
        const possible_get = window.location.href.split("/");
        let active;
        if (possible_get.includes("get")) {
            PollId = parseInt(possible_get.pop());
            active = ref(1);
        } else active = ref(0);
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
                    this.user.created = [];
                    this.user.taken = [];
                    // saving fingerprint to component's data
                    this.user.fingerprint = result.visitorId;
                }
                this.creatingPoll.startDate = new Date();
                // ------------- END LOADING ------------
                // exiting loading if we're are not GET-ing any poll
                if (PollId === null) {
                    this.$toast.success(Replies.loaded);
                    fetch(Requests.endpoints[this.AppMode] + "/warmup")
                        .then((res) => res.json())
                        .then((res) => this.$toast.info(res));
                    return;
                    // otherwise fetching poll passed as parameter
                }
                return (
                    fetch(
                        Requests.endpoints[this.AppMode] +
                            "/get/" +
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
        mypolls() {
            return this.user.created.length > 0 || this.user.taken.length > 0;
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
        makeReq(method, route, payload) {
            const e = Requests.endpoints[this.AppMode];
            const r = Requests.tryRoute(method, route);
            if (r === null) {
                this.$toast.error("Bad endpoint! Request aborted.");
                return;
            }
            const url = e + r;
            if (Requests.routes.get.some(i) === r)
                return fetch(url)
                    .catch((err) => this.$toast.error(err))
                    .then((res) => res.json());
            else {
                const missing_keys = Requests.post[route].filter(
                    (k) => !Object.keys(payload).includes(k)
                );
                if (missing_keys.length > 0) {
                    this.$toast.error(
                        "Missing key(s) from payload: " +
                            JSON.stringify(missing_keys)
                    );
                    return;
                }
                const config = {
                    headers: {
                        "Content-Type": "application/json",
                        Accept: "application/json",
                    },
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
            return this.makeReq("post", "ask_token", payload).then((res) => {
                this.$toast.success(res.resp_ask_token, {
                    duration: 15000,
                    dismissible: false,
                });
            });
        },
        confirmToken() {
            this.user.token_sent = true;
            const payload = {
                confirm_token: this.user.token,
                confirm_fingerprint: this.user.fingerprint,
                confirm_email: this.user.email,
            };
            return this.makeReq("post", "confirm_token", payload).then(
                (res) => {
                    if (res.resp_confirm_token && res.resp_confirm_hash) {
                        this.user.token = res.resp_confirm_token;
                        this.user.hash = res.resp_confirm_hash;
                        this.$toast.success(res.resp_confirm_msg);
                    } else {
                        this.$toast.warning(res.resp_confirm_msg);
                    }
                }
            );
        },
        createPoll() {
            let recipe = {
                poll_startDate: this.creatingPoll.startDate,
                poll_question: this.creatingPoll.question,
                poll_description: this.creatingPoll.description,
                poll_multiple: this.creatingPoll.multiple,
                poll_visible: this.creatingPoll.visible,
                poll_answers: this.creatingPoll.answers,
            };
            let payload = {
                create_hash: this.user.hash,
                create_token: this.user.token,
                create_startDate: this.creatingPoll.startDate,
            };
            if (this.creatingPoll.endDate !== null) {
                recipe.poll_endDate = this.creatingPoll.endDate;
                payload.create_endDate = this.creatingPoll.endDate;
            }
            payload.create_recipe = JSON.stringify(recipe);
            return this.makeReq("post", "create", payload).then((res) => {
                this.$toast.success(res.resp_create_msg);
                let createdPoll = {
                    question: this.creatingPoll.question,
                    startDate: this.creatingPoll.startDate,
                    link:
                        Requests[this.AppMode] +
                        "/" +
                        res.resp_create_pollid.toString(),
                };
                if (this.creatingPoll.endDate)
                    createdPoll.endDate = this.creatingPoll.endDate;
                this.user.created.push(createdPoll);
            });
        },
        closePoll() {
            const payload = {
                close_hash: this.user.hash,
                close_token: this.user.token,
                close_pollid: PollId,
            };
            return this.makeReq("post", "close", payload).then((res) =>
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
            return this.makeReq("post", "take", payload).then((res) =>
                this.$toast.success(res.resp_take_msg)
            );
        },
        restoreHistory() {
            const payload = {
                myhistory_hash: this.user.hash,
                myhistory_token: this.user.token,
            };
            this.user.created = [];
            this.user.taken = [];
            return this.makeReq("post", "myhistory", payload).then((res) => {
                if (res.resp_myhistory !== null) {
                    this.$toast.success(res.resp_myhistory_msg);
                    console.log("Attempting to decode pol...");
                    const mypolls = res.resp_myhistory_polls;
                    const created = res.resp_myhistory_created;
                    for (let [k, entry] of Object.entries(mypolls)) {
                        const poll = JSON.parse(entry[2][1]);
                        console.log(poll);
                        const startDate = entry[3][2];
                        const excerpt = {
                            question: poll.poll_question,
                            startDate: startDate,
                            link:
                                Requests[this.AppMode] + "/get/" + k.toString(),
                        };
                        if (created.includes(k))
                            this.user.created.push(excerpt);
                        else this.user.taken.push(excerpt);
                    }
                } else this.$toast.error(res.resp_myhistory_msg);
            });
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
