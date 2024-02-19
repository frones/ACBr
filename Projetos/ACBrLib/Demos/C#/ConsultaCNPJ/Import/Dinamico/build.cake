//******************************************************************************//
// Projeto: Componentes ACBr                                                    //
//  Biblioteca multiplataforma de componentes Delphi para interação com equipa- // 
// mentos de Automação Comercial utilizados no Brasil                           //
//                                                                              //
// Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               //
//                                                                              //
// Colaboradores nesse arquivo: Renato Rubinho                                  //
//                                                                              //
//  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    //
// Componentes localizado em      http://www.sourceforge.net/projects/acbr      //
//                                                                              //
//  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la //
// sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  //
// Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) //
// qualquer versão posterior.                                                   //
//                                                                              //
//  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   //
// NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      //
// ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor//
// do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              //
//                                                                              //
//  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto//
// com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  //
// no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          //
// Você também pode obter uma copia da licença em:                              //
// http://www.opensource.org/licenses/lgpl-license.php                          //
//                                                                              //
// Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br//
//       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         //
//******************************************************************************//

#addin nuget:?package=Cake.Incubator&version=7.0.0

var targetGetInfo = Argument("target", "GetInfo");
var targetBuild = Argument("target", "Build");
var targetPush = Argument("target", "PushNuget");
var configuration = Argument("configuration", "Release");
string projectFolder = "./";
string project = "";
string nugetPackage = "";
string tipoProjeto = "";
string nugetFolder = "bin/Release/";
string apiKey = EnvironmentVariable<string>("NUGET_APIKEY", "nao_encontrada");
string oldVersion = "";
string newVersion = "";
DotNetCoreMSBuildSettings msBuildSettings = new DotNetCoreMSBuildSettings();

Task("GetInfo")
    .Does(() => 
    {
        foreach (var item in GetFiles(projectFolder + tipoProjeto + "*.csproj"))
        {
            project = item.ToString();
        }

        if(project != "")
        {
            var props = ParseProject(project, "Release");
            oldVersion = props.GetProjectProperty("Version");
        }  
    });

Task("Clean")
    .Does(() => 
    {
        if(project == "")
            throw new Exception("Projeto não encontrado em:" + projectFolder + tipoProjeto);

        CleanDirectory(projectFolder + tipoProjeto + nugetFolder);
    });

Task("VersionAdd")
    .Does(() => 
    {
        var items = oldVersion.Split(".");
        int incVal = Int32.Parse(items[2]);
             
        if(incVal < 99999)
        {
            incVal += 1;
            items[2] = incVal.ToString();
        }
        else
        {
            items[2] = "0";
            incVal = Int32.Parse(items[1]);
             
            if(incVal < 99999)
            {
                incVal += 1;
                items[1] = incVal.ToString();
            }
            else
            {
                items[1] = "0";
                incVal = Int32.Parse(items[0]);
             
                incVal += 1;
                items[0] = incVal.ToString();
            }
        }

        newVersion = items[0] + "." + items[1] + "." + items[2] + ".0";
    });

Task("Restore")
    .IsDependentOn("GetInfo")
    .IsDependentOn("Clean")
    .IsDependentOn("VersionAdd")
    .Does(() => 
    {
        msBuildSettings
            .WithProperty("Version", newVersion)
            .WithProperty("AssemblyVersion", newVersion)
            .WithProperty("FileVersion", newVersion);

        DotNetCoreRestore(project, new DotNetCoreRestoreSettings
        {
            Verbosity = DotNetCoreVerbosity.Minimal,
            Sources = new [] { "https://api.nuget.org/v3/index.json" },
            MSBuildSettings = msBuildSettings
        });
    });

Task("Build")
    .IsDependentOn("Restore")
    .Does(() => 
    {
        DotNetCoreBuild(project, new DotNetCoreBuildSettings
        {
            NoRestore = true,
            Configuration = configuration,
            MSBuildSettings = msBuildSettings
        });

        string content = System.IO.File.ReadAllText(project);        

        content = content.Replace($"<Version>{oldVersion}</Version>", $"<Version>{newVersion}</Version>");
        content = content.Replace($"<AssemblyVersion>{oldVersion}</AssemblyVersion>", $"<AssemblyVersion>{newVersion}</AssemblyVersion>");
        content = content.Replace($"<FileVersion>{oldVersion}</FileVersion>", $"<FileVersion>{newVersion}</FileVersion>");

        System.IO.File.WriteAllText(project, content);       
    });

Task("PushNuget")
.IsDependentOn("GetInfo")
.Does(() =>
{
    foreach (var item in GetFiles(projectFolder + tipoProjeto + nugetFolder + "*.nupkg"))
    {
        nugetPackage = item.ToString();
        Information(nugetPackage);
        NuGetPush(nugetPackage, 
        new NuGetPushSettings {
            Source = "https://www.nuget.org/api/v2/package",
            ApiKey = apiKey
        });
    }
    if(nugetPackage == "")
        throw new Exception("Pacote nuget não encontrado em:" + projectFolder + tipoProjeto + nugetFolder);
});

if(apiKey == "nao_encontrada")
    throw new Exception("ApiKey não encontrada na variável de ambiente *NUGET_APIKEY*");

tipoProjeto = "";
RunTarget(targetGetInfo);

// Projetos na pasta raiz
if(project != "")
{
    RunTarget(targetBuild);
    RunTarget(targetPush);
}  
// Projetos em subpastas MT e ST
else
{
    // Roda as duas compilações
    tipoProjeto = "ST/";
    RunTarget(targetBuild);

    tipoProjeto = "MT/";
    RunTarget(targetBuild);

    // Se não ocorrerem erros, roda os dois pushs
    tipoProjeto = "ST/";
    RunTarget(targetPush);

    tipoProjeto = "MT/";
    RunTarget(targetPush);
}