FROM nginx:1.19.0

RUN rm /etc/nginx/conf.d/default.conf
COPY nginx_dev.conf /etc/nginx/conf.d/default.conf

RUN ln -sf /dev/stdout /var/log/nginx/access.log \
    && ln -sf /dev/stderr /var/log/nginx/error.log



RUN mkdir -p /opt/app/leatherbound_fe
COPY elm.js /opt/app/leatherbound_fe
COPY index.html /opt/app/leatherbound_fe
RUN mkdir -p /opt/app/leatherbound_fe/static
ADD static /opt/app/leatherbound_fe/static

EXPOSE 8000
