FROM dcurylo/fsharp-mono-netcore
VOLUME [ "/app" ]
ADD . app
WORKDIR app
RUN mono .paket/paket.exe install
RUN ./build.sh
RUN dotnet publish -f netcoreapp2.0 -c Release -o out
ENTRYPOINT [ "dotnet", "src/Kaleidoscope.Main/out/Kaleidoscope.dll" ]