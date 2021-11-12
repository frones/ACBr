#See https://aka.ms/containerfastmode to understand how Visual Studio uses this Dockerfile to build your images for faster debugging.

FROM mcr.microsoft.com/dotnet/aspnet:5.0-buster-slim AS base
RUN echo "deb http://deb.debian.org/debian buster main contrib\n" >> /etc/apt/sources.list &&\
    apt-get update && apt-get install -y --no-install-recommends \
    xvfb \
	xauth \
    openssl \    
    libxml2 \
    libgtk2.0-0 \
    ttf-mscorefonts-installer && \
    ln -s libxml2.so.2 libxml2.so
ENV DISPLAY :1
COPY Libs /usr/lib
WORKDIR /app
EXPOSE 80
EXPOSE 443

FROM mcr.microsoft.com/dotnet/sdk:5.0-buster-slim AS build
WORKDIR /src
COPY ["ACBr.API/ACBr.API.csproj", "ACBr.API/"]
RUN dotnet restore "ACBr.API/ACBr.API.csproj"
COPY . .
WORKDIR "/src/ACBr.API"
RUN dotnet build "ACBr.API.csproj" -c Release -o /app/build

FROM build AS publish
RUN dotnet publish "ACBr.API.csproj" -c Release -o /app/publish

FROM base AS final
WORKDIR /app
COPY --from=publish /app/publish .
ENTRYPOINT ["xvfb-run", "dotnet", "ACBr.API.dll"]