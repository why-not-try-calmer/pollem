<script>
export default {
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
    },
};
</script>