////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              PCN - Projeto Cooperar NFe                                    //
//                                                                            //
//   Descrição: Classes para geração/leitura dos arquivos xml da NFe          //
//                                                                            //
//        site: www.projetocooperar.org/nfe                                   //
//       email: projetocooperar@zipmail.com.br                                //
//       forum: http://br.groups.yahoo.com/group/projeto_cooperar_nfe/        //
//     projeto: http://code.google.com/p/projetocooperar/                     //
//         svn: http://projetocooperar.googlecode.com/svn/trunk/              //
//                                                                            //
// Coordenação: Paulo Casagrande                                              //
//                                                                            //
//      Equipe: Vide o arquivo leiame.txt na pasta raiz do projeto            //
//                                                                            //
//      Versão: Vide o arquivo leiame.txt na pasta raiz do projeto            //
//                                                                            //
//     Licença: GNU General Public License (GNU GPL)                          //
//                                                                            //
//              - Este programa é software livre; você pode redistribuí-lo    //
//              e/ou modificá-lo sob os termos da Licença Pública Geral GNU,  //
//              conforme publicada pela Free Software Foundation; tanto a     //
//              versão 2 da Licença como (a seu critério) qualquer versão     //
//              mais nova.                                                    //
//                                                                            //
//              - Este programa é distribuído na expectativa de ser útil,     //
//              mas SEM QUALQUER GARANTIA; sem mesmo a garantia implícita de  //
//              COMERCIALIZAÇÃO ou de ADEQUAÇÃO A QUALQUER PROPÓSITO EM       //
//              PARTICULAR. Consulte a Licença Pública Geral GNU para obter   //
//              mais detalhes. Você deve ter recebido uma cópia da Licença    //
//              Pública Geral GNU junto com este programa; se não, escreva    //
//              para a Free Software Foundation, Inc., 59 Temple Place,       //
//              Suite 330, Boston, MA - 02111-1307, USA ou consulte a         //
//              licença oficial em http://www.gnu.org/licenses/gpl.txt        //
//                                                                            //
//    Nota (1): - Esta  licença  não  concede  o  direito  de  uso  do nome   //
//              "PCN  -  Projeto  Cooperar  NFe", não  podendo o mesmo ser    //
//              utilizado sem previa autorização.                             //
//                                                                            //
//    Nota (2): - O uso integral (ou parcial) das units do projeto esta       //
//              condicionado a manutenção deste cabeçalho junto ao código     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//     Instruções                                                             //
//                                                                            //
// 1 - Crie uma pasta e coloque na o programa do validador                    //
//                                                                            //
//     ...\bin\validador                                                      //
//                                                                            //
// 2 - Crie sub-pastas com os schemas                                         //
//                                                                            //
//     ...\bin\validador\PL005C                                               //
//                                                                            //
// 3 - Parametros da função que chama o validador                             //
//                                                                            //
//     Mensagens: String - Variavel que recebera as mensagens de erro     //
//     PathArquivoXML: string - Caminho e o nome do arquivo xml               //
//     PathValidador: string - Caminho para pasta que esta o validador        //
//     Schema: TpcnSchema - Schema a ser utilizado                            //
//     TipoLayout: TpcnTipoLayout - Tipo do layout do arquivo                 //
//     ApagarAposValidar: Boolean - Opção para apagar o arquivo após validar  //
//                                                                            //
// 4 - Retorno                                                                //
//                                                                            //
//     Boolean - Retorna False se ocorrer algum erro                          //
//                                                                            //
// 5 - Conceito                                                               //
//                                                                            //
//     Permitir que outros validadores sejam integrados ao projeto utilizando //
//     outras tecnologias e linguagens                                        //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

{$I ACBr.inc}

unit pcnValidador;

interface

uses
  windows, SysUtils, Classes, pcnConversao;

function ValidarXML(var Mensagens: String;
                    PathArquivoXML,
                    PathValidador: String;
                    Schema: TpcnSchema;
                    TipoLayout: TpcnTipoLayout;
                    ApagarAposValidar: Boolean = False): Boolean;

implementation

function ValidarXML(var Mensagens: String; // Variavel que recebera as mensagens do validador
  PathArquivoXML, // Path e nome do arquivo xml a ser validado
  PathValidador: String; // Path aonde se encontra o .exe do validador
  Schema: TpcnSchema; // Schema que sera utilizada na validação
  TipoLayout: TpcnTipoLayout; // Tipo do layout do arquivo que sera validado
  ApagarAposValidar: Boolean = False): Boolean; // Indica se é para apagar o xml após validar
var
  i: Integer;
  ArquivoRetorno: TStringList;
  NomeArquivoRetorno: String;
begin
  Result := False;
  if not FileExists(PathArquivoXML) then
  begin
    Mensagens := Mensagens + 'O Arquivo [ ' + Trim(PathArquivoXML) + ' ] não foi encontrado!';
    exit;
  end;
  if pos(' ', PathValidador) > 0 then
  begin
    Mensagens := Mensagens + 'Não é permitido usar nome de diretórios com espaço(s) em branco [ ' + Trim(PathValidador) + ' ]';
    exit;
  end;
  if pos(' ', PathArquivoXML) > 0 then
  begin
    Mensagens := Mensagens + 'Não é permitido usar nome de diretórios com espaço(s) em branco [ ' + Trim(PathArquivoXML) + ' ]';
    exit;
  end;
  Randomize;
  i := Random(999999999);
  NomeArquivoRetorno := PathValidador + 'R' + IntToStr(i) + '.TXT';
  try
    WinExec(pAnsiChar(PathValidador + 'pcnValidadorNFe.exe ' +
      PathArquivoXML + ' ' +
      PathValidador + SchemaToStr(Schema) + '\ ' +
      NomeArquivoRetorno + ' ' +
      TipoLayoutToStr(TipoLayout)),
      sw_hide);
    while not FileExists(NomeArquivoRetorno) do begin end;
    sleep(500);
    ArquivoRetorno := TStringList.Create;
    ArquivoRetorno.LoadFromFile(NomeArquivoRetorno);
    Mensagens := ArquivoRetorno.Text;
    ArquivoRetorno.Free;
    DeleteFile(NomeArquivoRetorno);
    if ApagarAposValidar then
      DeleteFile(PathArquivoXML);
    Result := Mensagens = '';
  except
    Mensagens := Mensagens + 'Não foi possivel validar o arquivo.';
    Result := False;
  end;
end;

end.

