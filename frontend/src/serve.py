#!/usr/bin/env python3

from datetime import datetime, timedelta
from flask import Flask, Response, request, render_template, send_from_directory

from faker import Faker
import faker.providers

import os
import random
import requests
import urllib.parse

API_URI = os.getenv("API_URI", "https://event-api.barrucadu.dev")
BASE_URI = os.getenv("BASE_URI", "https://www.barrucadu.dev")

FAKE = Faker()
FAKE.add_provider(faker.providers.date_time)
FAKE.add_provider(faker.providers.internet)
FAKE.add_provider(faker.providers.lorem)
FAKE.add_provider(faker.providers.misc)
FAKE.add_provider(faker.providers.python)

app = Flask(__name__)


def fetch_events_from_api(count=150, project=None):
    def clean_dict(d):
        cleaned = {k: v for k, v in d.items() if v is not None}
        for k, v in cleaned.items():
            if isinstance(v, dict):
                cleaned[k] = clean_dict(v)
        return cleaned

    if project is None:
        r = requests.get(f"{API_URI}/events", params={"count": count})
    else:
        r = requests.get(
            f"{API_URI}/project/{urllib.parse.quote(project)}/events",
            params={"count": count},
        )
        if r.status_code == 404:
            return []
    r.raise_for_status()
    return [clean_dict(event) for event in r.json()]


def phony_events(count=150, project=None):
    def phony_project():
        return {
            "name": "-".join(FAKE.words(nb=random.randint(1, 3), unique=True)),
            "url": FAKE.image_url(),
        }

    def phony_event(projects):
        event = {
            "project": random.choice(projects),
            "uuid": FAKE.uuid4(),
            "timestamp": FAKE.date_time_between(
                start_date="-13d", end_date="now"
            ).strftime("%Y-%m-%dT%H:%M:%S.%fZ"),
            "status": random.choice(
                ["Ok", "Ok", "Ok", "Ok", "Failure", "Failure", "Error"]
            ),
            "description": FAKE.sentence(),
        }
        if FAKE.boolean(chance_of_getting_true=75):
            event["phase"] = FAKE.word()
        if FAKE.boolean(chance_of_getting_true=75):
            event["tag"] = FAKE.sha256()[:7]
            if FAKE.boolean(chance_of_getting_true=75):
                event["tagUrl"] = FAKE.image_url()
        if FAKE.boolean(chance_of_getting_true=75):
            event["detailsUrl"] = FAKE.image_url()
        return event

    if project is None:
        projects = [phony_project() for i in range(random.randint(1, 10))]
    else:
        projects = [{"name": project, "url": FAKE.image_url()}]

    return sorted(
        [phony_event(projects) for i in range(0, count)],
        key=lambda event: event["timestamp"],
        reverse=True,
    )


def munge_event(now, event):
    then = datetime.strptime(event["timestamp"], "%Y-%m-%dT%H:%M:%S.%fZ")
    offset = now - then
    if offset < timedelta(minutes=1):
        event["relative_timestamp"] = "now"
    elif offset < timedelta(minutes=2):
        event["relative_timestamp"] = "1 minute ago"
    elif offset < timedelta(hours=1):
        event["relative_timestamp"] = f"{offset // timedelta(minutes=1)} minutes ago"
    elif offset < timedelta(hours=2):
        event["relative_timestamp"] = "1 hour ago"
    elif offset < timedelta(days=1):
        event["relative_timestamp"] = f"{offset // timedelta(hours=1)} hours ago"
    elif offset < timedelta(days=2):
        event["relative_timestamp"] = "yesterday"
    elif offset < timedelta(days=6):
        event["relative_timestamp"] = f"on {then.strftime('%A')}"
    else:
        event["relative_timestamp"] = then.strftime("%b %d")

    event["status_image"] = f"status-{event['status'].lower()}.png"

    if "phase" in event:
        event["phase"] = event["phase"].capitalize()


def get_events(request_args):
    project = request_args.get("project")

    if "phony" in request_args:
        events = phony_events(project=project)
    else:
        events = fetch_events_from_api(project=project)

    now = datetime.now()
    for event in events:
        munge_event(now, event)

    return events


@app.route("/")
def index():
    events = get_events(request.args)

    return render_template("index.html", base_uri=BASE_URI, events=events)


@app.route("/atom.xml")
def feed():
    events = get_events(request.args)

    feed_date = None
    if len(events) > 0:
        feed_date = events[0]["timestamp"]

    return Response(
        render_template(
            "atom.xml", base_uri=BASE_URI, events=events, feed_date=feed_date
        ),
        content_type="application/atom+xml",
    )


@app.route("/static/<path>")
def static_files(path):
    return send_from_directory("static", path)


app.run(host="0.0.0.0", port=3000)
