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

unit ACBrLibPosPrinterTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

const
  CLibPosPrinterNome = 'ACBrLibPosPrinter';

type

  { TTestACBrPosPrinterLib }

  TTestACBrPosPrinterLib = class(TTestCase)
  private
    procedure ConfigurarImpressora(APorta: String = ''; AModelo: String = '');
    function GetPrinterPort: String;
    function GetPrinterModel: String;
    function GetPaginaCodigo: String;
  published
    procedure Test_POS_Inicializar_Com_DiretorioValido;
    procedure Test_POS_Inicializar_Com_DiretorioInvalido;
    procedure Test_POS_Inicializar;
    procedure Test_POS_Inicializar_Ja_Inicializado;
    procedure Test_POS_Finalizar;
    procedure Test_POS_Finalizar_Ja_Finalizado;
    procedure Test_POS_Nome_Obtendo_LenBuffer;
    procedure Test_POS_Nome_Lendo_Buffer_Tamanho_Identico;
    procedure Test_POS_Nome_Lendo_Buffer_Tamanho_Maior;
    procedure Test_POS_Nome_Lendo_Buffer_Tamanho_Menor;
    procedure Test_POS_Versao;
    procedure Test_POS_ConfigLerValor;
    procedure Test_POS_ConfigGravarValor;
    procedure Test_POS_InicializarConfigGravarValoresEFinalizar;
    procedure Test_POS_InicializarAtivarEFinalizar;
    procedure Test_POS_ImpressaoDeTags;
    procedure Test_POS_RetornarEInterpretarTags;
    procedure Test_POS_ImprimirAcentos;
  end;

implementation

uses
  ACBrLibPosPrinterStaticImport, ACBrLibConsts, ACBrUtil;

function TTestACBrPosPrinterLib.GetPrinterPort: String;
begin
  Result := 'COM21'; //ApplicationPath+'posprinter.txt';
end;

function TTestACBrPosPrinterLib.GetPrinterModel: String;
begin
  Result := '2';

  //0 = ppTexto (Padrão)
  //1 = ppEscPosEpson
  //2 = ppEscBematec
  //3 = ppEscDaruma
  //4 = ppEscVox
  //5 = ppEscDiebold
  //6 = ppEscEpsonP2
  //7 = ppCustomPos
  //8 = ppEscPosStar
  //9 = ppEscZJiang
  //10 = ppEscGPrinter
end;

function TTestACBrPosPrinterLib.GetPaginaCodigo: String;
begin
  Result := '2';
 //0 = pcNone
 //1 = pc437
 //2 = pc850 (Padrão)
 //3 = pc852
 //4 = pc860
 //5 = pcUTF8
 //6 = pc1252
end;

procedure TTestACBrPosPrinterLib.ConfigurarImpressora(APorta: String;
  AModelo: String);
var
  SaidaImpressao, Modelo, PagCod: String;
begin
  if APorta <> '' then
    SaidaImpressao := APorta
  else
    SaidaImpressao := GetPrinterPort;

  if AModelo <> '' then
    Modelo := AModelo
  else
    Modelo := GetPrinterModel;

  PagCod := GetPaginaCodigo;

  AssertEquals(ErrOK, POS_ConfigGravarValor(CSessaoPosPrinter, CChaveModelo, PChar(Modelo)));
  AssertEquals(ErrOK, POS_ConfigGravarValor(CSessaoPosPrinter, CChavePorta, PChar(SaidaImpressao)));
  AssertEquals(ErrOK, POS_ConfigGravarValor(CSessaoPosPrinter, CChavePaginaDeCodigo, PChar(PagCod)));
  AssertEquals(ErrOK, POS_ConfigGravar(''));
  AssertEquals(ErrOK, POS_ConfigLer(''));
end;

procedure TTestACBrPosPrinterLib.Test_POS_Inicializar_Com_DiretorioValido;
var
  fileName: String;
begin
  fileName := ApplicationPath+'ACBrLib.ini';
  if FileExists(fileName) then
    DeleteFile(fileName);

  try
    AssertEquals(ErrOK, POS_Inicializar( PChar(fileName),''));
    AssertTrue(FileExists(fileName));
    AssertEquals(ErrOK, POS_Finalizar);
  finally
    DeleteFile(fileName);
  end;
end;

procedure TTestACBrPosPrinterLib.Test_POS_Inicializar_Com_DiretorioInvalido;
begin
  POS_Finalizar();
  AssertEquals(ErrDiretorioNaoExiste, POS_Inicializar('C:\NAOEXISTE\ACBrLib.ini',''));
end;

procedure TTestACBrPosPrinterLib.Test_POS_Inicializar;
begin
  AssertEquals(ErrOk, POS_Inicializar('',''));
end;

procedure TTestACBrPosPrinterLib.Test_POS_Inicializar_Ja_Inicializado;
begin
  AssertEquals(ErrOk, POS_Inicializar('',''));
end;

procedure TTestACBrPosPrinterLib.Test_POS_Finalizar;
begin
  AssertEquals(ErrOk, POS_Finalizar());
end;

procedure TTestACBrPosPrinterLib.Test_POS_Finalizar_Ja_Finalizado;
begin
  AssertEquals(ErrOk, POS_Finalizar());
end;

procedure TTestACBrPosPrinterLib.Test_POS_Nome_Obtendo_LenBuffer;
var
  Bufflen: Integer;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, POS_Nome(Nil, Bufflen));
  AssertEquals(Length(CLibPosPrinterNome), Bufflen);
end;

procedure TTestACBrPosPrinterLib.Test_POS_Nome_Lendo_Buffer_Tamanho_Identico;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibPosPrinterNome);
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, POS_Nome(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibPosPrinterNome), Bufflen);
  AssertEquals(CLibPosPrinterNome, AStr);
end;

procedure TTestACBrPosPrinterLib.Test_POS_Nome_Lendo_Buffer_Tamanho_Maior;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibPosPrinterNome)*2;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, POS_Nome(PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(Length(CLibPosPrinterNome), Bufflen);
  AssertEquals(CLibPosPrinterNome, AStr);
end;

procedure TTestACBrPosPrinterLib.Test_POS_Nome_Lendo_Buffer_Tamanho_Menor;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := 4;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, POS_Nome(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibPosPrinterNome), Bufflen);
  AssertEquals(copy(CLibPosPrinterNome,1,4), AStr);
end;

procedure TTestACBrPosPrinterLib.Test_POS_Versao;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, POS_Versao(Nil, Bufflen));
  Assert(Bufflen > 0);

  // Lendo a resposta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, POS_Versao(PChar(AStr), Bufflen));
  Assert(Bufflen > 0);
  Assert(AStr <> '');
end;

procedure TTestACBrPosPrinterLib.Test_POS_ConfigLerValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, POS_ConfigLerValor(CSessaoVersao, CACBrLib, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals(CACBrLibVersaoConfig, AStr);
end;

procedure TTestACBrPosPrinterLib.Test_POS_ConfigGravarValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Gravando o valor
  AssertEquals('Erro ao Mudar configuração', ErrOk, POS_ConfigGravarValor(CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, POS_ConfigLerValor(CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '4', AStr);
end;

procedure TTestACBrPosPrinterLib.Test_POS_InicializarConfigGravarValoresEFinalizar;
begin
  AssertEquals(ErrOk, POS_Inicializar('',''));
  ConfigurarImpressora;
  AssertEquals(ErrOK, POS_Ativar);

  AssertEquals(ErrOK, POS_Finalizar());
end;

procedure TTestACBrPosPrinterLib.Test_POS_InicializarAtivarEFinalizar;
var
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, POS_Inicializar('',''));
  ConfigurarImpressora('COM99','');
  AssertEquals(ErrExecutandoMetodo, POS_Ativar);

  // Checando se é possivel pegar a descrição do erro //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrExecutandoMetodo, POS_UltimoRetorno(PChar(AStr), bufflen));
  AssertEquals('Porta não definida', Trim(AStr));

  AssertEquals(ErrOK, POS_Finalizar());
end;

procedure TTestACBrPosPrinterLib.Test_POS_ImpressaoDeTags;
begin
  AssertEquals(ErrOk, POS_Inicializar('',''));
  ConfigurarImpressora;
  AssertEquals(ErrOK, POS_Ativar);
  AssertEquals(ErrOK, POS_ImprimirTags);
  AssertEquals(ErrOK, POS_Desativar);
  AssertEquals(ErrOK, POS_Finalizar());
end;

procedure TTestACBrPosPrinterLib.Test_POS_RetornarEInterpretarTags;
var
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, POS_Inicializar('',''));
  ConfigurarImpressora;
  AssertEquals(ErrOK, POS_Ativar);
  Bufflen := 0;   // chama com Zero, para achar o tamanho do Buffer
  AStr := '';
  AssertEquals(ErrOK, POS_RetornarTags(False, PChar(AStr), Bufflen));
  AStr := Space(Bufflen);
  AssertEquals(ErrOK, POS_UltimoRetorno(PChar(AStr), Bufflen));  // Chama UltimoRetorno, Para não processar Tags novamente
  AssertEquals(copy(AStr,1,133), '<e>|</e>|<a>|</a>|<n>|</n>|<s>|</s>|<c>|</c>|<i>|</i>|</fn>|</fa>|</fb>|<in>|</in>|</ae>|</ce>|</ad>|</linha_simples>|</linha_dupla>|');
  AssertEquals(ErrOK, POS_Imprimir(PChar(AStr), True, True, True, 1));
  AssertEquals(ErrOK, POS_Desativar);
  AssertEquals(ErrOK, POS_Finalizar());
end;

procedure TTestACBrPosPrinterLib.Test_POS_ImprimirAcentos;
var
  AStr: String;
begin
  AStr := 'Áá Éé Íí Óó Úú Çç Ââ Êê Îî Ôo Ûû';    // Essa UNIT está em UTF8
  AssertEquals(ErrOk, POS_Inicializar('',''));
  ConfigurarImpressora;
  AssertEquals(ErrOK, POS_Ativar);
  AssertEquals(ErrOK, POS_Imprimir(PChar(Astr), True, True, True, 1) );
  AssertEquals(ErrOK, POS_PularLinhas(7) );
  AssertEquals(ErrOK, POS_Desativar);
  AssertEquals(ErrOK, POS_Finalizar());
end;

initialization
  RegisterTest(TTestACBrPosPrinterLib);

end.

