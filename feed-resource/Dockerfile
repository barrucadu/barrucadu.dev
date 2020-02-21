FROM python:3.8

COPY requirements-freeze.txt .
RUN pip install -r requirements-freeze.txt

COPY ./src/* /opt/resource/
