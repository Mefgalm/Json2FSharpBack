FROM mcr.microsoft.com/dotnet/aspnet:6.0

COPY Json2FSharpBack/bin/Release/net6.0/publish/ App/
WORKDIR /App
ENTRYPOINT ["dotnet", "DotNet.Docker.dll"]