{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Rafael Teno Dias                                }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

unit ACBrLibNCMsTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

const
  CLibNCMsNome = 'ACBrLibNCMs';


type

  { TTestACBrNCMsLib }

  TTestACBrNCMsLib = class(TTestCase)
  published
    procedure Test_NCMs_Inicializar_Com_DiretorioInvalido;
    procedure Test_NCMs_Inicializar;
    procedure Test_NCMs_Inicializar_Ja_Inicializado;
    procedure Test_NCMs_Finalizar;
    procedure Test_NCMs_Finalizar_Ja_Finalizado;
    procedure Test_NCMs_Nome_StringCorreta;
    procedure Test_NCMs_Nome_Obtendo_LenBuffer;
    procedure Test_NCMs_Nome_Lendo_Buffer_Tamanho_Identico;
    procedure Test_NCMs_Nome_Lendo_Buffer_Tamanho_Maior;
    procedure Test_NCMs_Nome_Lendo_Buffer_Tamanho_Menor;
    procedure Test_NCMs_Versao;
    procedure Test_NCMs_ConfigLerValor;
    procedure Test_NCMs_ConfigGravarValor;

    procedure Test_NCM_ObterNCMs;
    procedure Test_NCM_BaixarLista;

    procedure Test_NCM_DescricaoNCM_RetornaValor;
    procedure Test_NCM_BuscarPorCodigo_RetornaValor;
    procedure Test_NCM_BuscarPorDescricao_RetornaValor;
    procedure Test_NCM_BuscarPorDescricaoJSON;
    procedure Test_NCM_BuscarPorDescricaoXML;

  end;

implementation

uses
  Dialogs, ACBrUtil.Strings, ACBrLibComum, ACBrLibConsts, ACBrLibNCMsStaticImportMT;

const
  cCodigoNCMValido01 = '84719012';
  cDescricaoNCMValida01 = 'Leitores de códigos de barras';
  cDataInicioVigecia01 = '01/04/2022';
  cDataFinalVigencia01 = '31/12/9999';
  cTipoAto01 = 'Res Camex';
  cNumeroAto01 = '000272';
  AnoAto01 = '2021';
  cRespostaCompleta01 = cCodigoNCMValido01 + '|'+ cDescricaoNCMValida01 +'|'+cDataInicioVigecia01 +'|'+ cDataFinalVigencia01 +
                      '|'+ cTipoAto01 +'|'+ cNumeroAto01 +'|'+ AnoAto01;

  cDescricaoParcial01 = 'Cavalo';
  cRetornoMultiplo01 =  '0101|Cavalos, asininos e muares, vivos.|01/04/2022|31/12/9999|Res Camex|000272|2021'+ sLineBreak +
                        '01012|Cavalos:|01/04/2022|31/12/9999|Res Camex|000272|2021';

procedure TTestACBrNCMsLib.Test_NCMs_Inicializar_Com_DiretorioInvalido;
var
  Handle: PLibHandle;
begin
    AssertEquals(ErrDiretorioNaoExiste, NCM_Inicializar(Handle, 'C:\.NAO.EXISTE\ACBrLib.ini',''));
end;

procedure TTestACBrNCMsLib.Test_NCMs_Inicializar;
var
  Handle: PLibHandle;
begin
  AssertEquals(ErrOk, NCM_Inicializar(Handle, '',''));
  AssertEquals(ErrOK, NCM_Finalizar(Handle));
end;

procedure TTestACBrNCMsLib.Test_NCMs_Inicializar_Ja_Inicializado;
var
  Handle: PLibHandle;
begin
  AssertEquals(ErrOk, NCM_Inicializar(Handle, '',''));
  AssertEquals(ErrOk, NCM_Inicializar(Handle, '',''));
  AssertEquals(ErrOK, NCM_Finalizar(Handle));
end;

procedure TTestACBrNCMsLib.Test_NCMs_Finalizar;
var
  Handle: PLibHandle;
begin
  AssertEquals(ErrOk, NCM_Inicializar(Handle, '',''));
  AssertEquals(ErrOk, NCM_Finalizar(Handle));
end;

procedure TTestACBrNCMsLib.Test_NCMs_Finalizar_Ja_Finalizado;
var
  Handle: PLibHandle;
begin

  AssertEquals(ErrOk, NCM_Inicializar(Handle, '',''));
  AssertEquals(ErrOk, NCM_Finalizar(Handle));

  try
    AssertEquals(ErrOk, NCM_Inicializar(Handle, '',''));
    AssertEquals(ErrOk, NCM_Finalizar(Handle));
    //AssertEquals(ErrOk, NCM_Finalizar(Handle));
  except
  on E: Exception do
     ShowMessage('Error: '+ E.ClassName + #13#10 + E.Message);
  end;
end;

procedure TTestACBrNCMsLib.Test_NCMs_Nome_StringCorreta;
var
  Handle: PLibHandle;
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOk, NCM_Inicializar(Handle, '',''));
  Bufflen := 0;
  AssertEquals(ErrOk, NCM_Nome(Handle, nil, Bufflen));
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, NCM_Nome(Handle, PChar(AStr), Bufflen));
  AssertEquals(CLibNCMsNome, AStr);
  AssertEquals(ErrOk, NCM_Finalizar(Handle));
end;

procedure TTestACBrNCMsLib.Test_NCMs_Nome_Obtendo_LenBuffer;
var
  Handle: PLibHandle;
  Bufflen: Integer;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOk, NCM_Inicializar(Handle, '',''));
  Bufflen := 0;
  AssertEquals(ErrOk, NCM_Nome(Handle, nil, Bufflen));
  AssertEquals(Length(CLibNCMsNome), Bufflen);
  AssertEquals(ErrOk, NCM_Finalizar(Handle));
end;

procedure TTestACBrNCMsLib.Test_NCMs_Nome_Lendo_Buffer_Tamanho_Identico;
var
  Handle: PLibHandle;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOk, NCM_Inicializar(Handle, '',''));
  Bufflen := Length(CLibNCMsNome);
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, NCM_Nome(Handle, PChar(AStr), Bufflen));
  AssertEquals(Length(CLibNCMsNome), Bufflen);
  AssertEquals(CLibNCMsNome, AStr);
  AssertEquals(ErrOk, NCM_Finalizar(Handle));
end;

procedure TTestACBrNCMsLib.Test_NCMs_Nome_Lendo_Buffer_Tamanho_Maior;
var
  Handle: PLibHandle;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOk, NCM_Inicializar(Handle, '',''));
  Bufflen := Length(CLibNCMsNome)*2;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, NCM_Nome(Handle, PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(Length(CLibNCMsNome), Bufflen);
  AssertEquals(CLibNCMsNome, AStr);
  AssertEquals(ErrOk, NCM_Finalizar(Handle));
end;

procedure TTestACBrNCMsLib.Test_NCMs_Nome_Lendo_Buffer_Tamanho_Menor;
var
  Handle: PLibHandle;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOk, NCM_Inicializar(Handle, '',''));
  Bufflen := 4;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, NCM_Nome(Handle, PChar(AStr), Bufflen));
  AssertEquals(Length(CLibNCMsNome), Bufflen);
  AssertEquals(copy(CLibNCMsNome,1,4), AStr);
  AssertEquals(ErrOk, NCM_Finalizar(Handle));
end;

procedure TTestACBrNCMsLib.Test_NCMs_Versao;
var
  Handle: PLibHandle;
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOk, NCM_Inicializar(Handle, '',''));
  Bufflen := 0;
  AssertEquals(ErrOk, NCM_Versao(Handle, Nil, Bufflen));
  Assert(Bufflen > 0);

  // Lendo a resposta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, NCM_Versao(Handle, PChar(AStr), Bufflen));
  Assert(Bufflen > 0);
  Assert (AStr <> '');
  AssertEquals(ErrOk, NCM_Finalizar(Handle));
end;

procedure TTestACBrNCMsLib.Test_NCMs_ConfigLerValor;
var
  Handle: PLibHandle;
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOk, NCM_Inicializar(Handle, '',''));
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, NCM_ConfigLerValor(Handle, CSessaoVersao, CACBrLib, PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(CACBrLibVersaoConfig, AStr);
  AssertEquals(ErrOk, NCM_Finalizar(Handle));
end;

procedure TTestACBrNCMsLib.Test_NCMs_ConfigGravarValor;
var
  Handle: PLibHandle;
  Bufflen: Integer;
  AStr: String;
begin
  // Gravando o valor
  AssertEquals(ErrOk, NCM_Inicializar(Handle, '',''));
  AssertEquals('Erro ao Mudar configuração', ErrOk, NCM_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, NCM_ConfigLerValor(Handle, CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);

  AssertEquals('Erro ao Mudar configuração', '4', AStr);
  AssertEquals(ErrOk, NCM_Finalizar(Handle));
end;

procedure TTestACBrNCMsLib.Test_NCM_ObterNCMs;
const
  SNomeArquivoACBrNCMJson = 'ACBrNCM.json';
var
  Handle: PLibHandle;
  Resposta: String;
  Tamanho: Longint;
begin
  Resposta := Space  (24);
  Tamanho := 24;

  if FileExists(SNomeArquivoACBrNCMJson) then
  begin
    DeleteFile(SNomeArquivoACBrNCMJson);
  end;

  AssertEquals(ErrOk, NCM_Inicializar(Handle, '',''));

  AssertEquals('Erro ao BaixarLista', ErrOk,
    NCM_ObterNCMs(Handle, PChar(Resposta), Tamanho));

  CheckEquals('Lista de NCMs atualizada', Resposta, Resposta);
  //AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

  CheckTrue(FileExists(SNomeArquivoACBrNCMJson), 'Arquivo "'+SNomeArquivoACBrNCMJson+'" não foi criado na pasta do executável');

  AssertEquals(ErrOk, NCM_Finalizar(Handle));
end;

procedure TTestACBrNCMsLib.Test_NCM_BaixarLista;
var
  Handle: PLibHandle;
  Resposta: String;
  Tamanho: Longint;
  ArquivoTemporario: string;
const
  SmsgArquivoSalvoEm = 'Arquivo salvo em: ';
begin
  ArquivoTemporario := ExtractFilePath(ParamStr(0)) + 'NCMsTemp.csv';

  Tamanho := Length(SmsgArquivoSalvoEm) + Length(ArquivoTemporario);
  Resposta := Space(Tamanho);

  AssertEquals(ErrOk, NCM_Inicializar(Handle, '',''));
  //AssertEquals('Erro ao Mudar configuração', ErrOk, NCM_ConfigGravarValor(Handle, CSessaoCEP, CChaveWebService, '10'));

  AssertEquals('Erro ao BaixarLista', ErrOk,
    NCM_BaixarLista(Handle, PChar(ArquivoTemporario), PChar(Resposta), Tamanho));
  CheckTrue(FileExists(ArquivoTemporario), 'Arquivo não foi criado na pasta definida: '+ ArquivoTemporario);

  AssertEquals('Resposta= ' + AnsiString(Resposta), SmsgArquivoSalvoEm+ArquivoTemporario, Resposta);
  //AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
  AssertEquals(ErrOk, NCM_Finalizar(Handle));
end;

procedure TTestACBrNCMsLib.Test_NCM_DescricaoNCM_RetornaValor;
var
  Handle: PLibHandle;
  Resposta: String;
  Tamanho: Longint;
  RetornoFuncao: LongInt;
begin
  //Tamanho := Length(cDescricaoNCMValida01);
  //Resposta := space(Tamanho);
  Tamanho := 0;

  AssertEquals(ErrOk, NCM_Inicializar(Handle, '',''));
  //AssertEquals('Erro ao Mudar configuração', ErrOk, NCM_ConfigGravarValor(Handle, CSessaoCEP, CChaveWebService, '10'));  // Via Cep
  AssertEquals('Erro pegar tamanho da resposta', ErrOk,
    NCM_DescricaoNCM(Handle, PChar(cCodigoNCMValido01),
                            nil, Tamanho));

  Resposta := space(Tamanho);
  RetornoFuncao := NCM_DescricaoNCM(Handle, PChar(cCodigoNCMValido01), PChar(Resposta), Tamanho);
  if RetornoFuncao < 0 then
  begin
    Resposta := Space(Tamanho);
    NCM_UltimoRetorno(Handle, Pchar(Resposta), Tamanho );
  end;
  AssertEquals('Erro ao buscar descrição do NCM: '+ Resposta, ErrOk, RetornoFuncao);

  AssertEquals('Resposta= ' + AnsiString(Resposta), cDescricaoNCMValida01, Resposta);
  //AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
  AssertEquals(ErrOk, NCM_Finalizar(Handle));
end;

procedure TTestACBrNCMsLib.Test_NCM_BuscarPorCodigo_RetornaValor;
var
  Handle: PLibHandle;
  Resposta: String;
  Tamanho: Longint;
begin
  Tamanho := ByteLength(cRespostaCompleta01);
  Resposta := Space(Tamanho);

  AssertEquals(ErrOk, NCM_Inicializar(Handle, '',''));
  //AssertEquals('Erro ao Mudar configuração', ErrOk, NCM_ConfigGravarValor(Handle, CSessaoCEP, CChaveWebService, '10'));  // Via Cep
  AssertEquals('Erro ao buscar descrição do NCM', ErrOk,
    NCM_BuscarPorCodigo(Handle, PChar(cCodigoNCMValido01),
                            PChar(Resposta), Tamanho));

  AssertEquals('Resposta= ' + AnsiString(Resposta), cRespostaCompleta01, trim(Resposta));
  //AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
  AssertEquals(ErrOk, NCM_Finalizar(Handle));
end;

procedure TTestACBrNCMsLib.Test_NCM_BuscarPorDescricao_RetornaValor;
var
  Handle: PLibHandle;
  Resposta: String;
  Tamanho: Longint;
  RetornoFuncao: LongInt;
begin
  Resposta := space(10240);
  Tamanho := 10240;

  AssertEquals(ErrOk, NCM_Inicializar(Handle, '',''));

  RetornoFuncao := NCM_BuscarPorDescricao(Handle, PChar(cDescricaoNCMValida01), 1, PChar(Resposta), Tamanho);

  if RetornoFuncao <0 then
  begin
    Resposta := Space(Tamanho);
    NCM_UltimoRetorno(Handle, Pchar(Resposta), Tamanho );
  end;
  AssertEquals('Erro ao buscar descrição do NCM', ErrOk, RetornoFuncao);

  CheckNotEquals('', trim(Resposta), 'Resposta= ' + AnsiString(Resposta));
  //AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
  AssertEquals(ErrOk, NCM_Finalizar(Handle));
end;

procedure TTestACBrNCMsLib.Test_NCM_BuscarPorDescricaoJSON;
var
  Handle: PLibHandle;
  Qtde: Integer;
  Resposta: string;
  Tamanho: Longint;
  Retorno: PChar;
begin
  Tamanho := Length(cRetornoMultiplo01);
  Resposta := space(Tamanho);

  AssertEquals(ErrOk, NCM_Inicializar(Handle, '',''));

  NCM_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveTipoResposta, '2');  // 2 -json
  //NCM_ConfigGravarValor(Handle, 'CEP', 'WebService', '11');

  Qtde := NCM_BuscarPorDescricao(Handle, cDescricaoParcial01, 1, pchar(Resposta), Tamanho);

  if Qtde <0 then
  begin
    Retorno := PChar(Space(Tamanho));
    NCM_UltimoRetorno(Handle, Retorno, Tamanho );
  end;
  AssertEquals('Erro ao buscar descrição do NCM', ErrOk, Qtde);

  //AssertEquals('Qtde = ' + IntToStr(Qtde), '', '');
  AssertEquals('Resposta= ' + AnsiString(Resposta), cRetornoMultiplo01, trim(Resposta));
  //AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
  AssertEquals(ErrOk, NCM_Finalizar(Handle));
end;

procedure TTestACBrNCMsLib.Test_NCM_BuscarPorDescricaoXML;
var
  Handle: PLibHandle;
  RetornoDaFuncao: Integer;
  Resposta: string;
  Tamanho: Longint;
  Retorno: PChar;
begin
  // Buscando o Endereço por CEP
  Resposta := space(10240);
  Tamanho := 10240;

  AssertEquals(ErrOk, NCM_Inicializar(Handle, '',''));

  NCM_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveTipoResposta, '1');  // 2 -json
  //NCM_ConfigGravarValor(Handle, 'CEP', 'WebService', '11');

  RetornoDaFuncao := NCM_BuscarPorDescricao(Handle, cDescricaoParcial01, 1, pchar(Resposta), Tamanho);

  if RetornoDaFuncao < 0 then
  begin
    Retorno := PChar(Space(Tamanho));
    NCM_UltimoRetorno(Handle, Retorno, Tamanho );
  end;
  AssertEquals('Erro ao fazer busca por descricao', ErrOk, RetornoDaFuncao);


  if RetornoDaFuncao > 0 then
  begin
    AssertEquals('Qtde = ' + IntToStr(RetornoDaFuncao), '', '');
    AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
    AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
  end;
  AssertEquals(ErrOk, NCM_Finalizar(Handle));
end;

initialization
  RegisterTest(TTestACBrNCMsLib);

end.

