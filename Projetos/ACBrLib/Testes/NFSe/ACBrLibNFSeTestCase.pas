{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Antonio Carlos Junior                           }
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

unit ACBrLibNFSeTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TTestACBrNFSeLib }

  TTestACBrNFSeLib= class(TTestCase)
  private
    fCaminhoExec: string;
  public
    procedure SetUp; override;
  published
    procedure Test_NFSE_Inicializar_Com_DiretorioInvalido;
    procedure Test_NFSE_Inicializar;
    procedure Test_NFSE_Inicializar_Ja_Inicializado;
    procedure Test_NFSE_Finalizar;
    procedure Test_NFSE_Finalizar_Ja_Finalizado;
    procedure Test_NFSE_Nome_Obtendo_LenBuffer;
    procedure Test_NFSE_Nome_Lendo_Buffer_Tamanho_Identico;
    procedure Test_NFSE_Nome_Lendo_Buffer_Tamanho_Maior;
    procedure Test_NFSE_Nome_Lendo_Buffer_Tamanho_Menor;
    procedure Test_NFSE_Versao;
    procedure Test_NFSE_ConfigLerValor;
    procedure Test_NFSE_ConfigGravarValor;
    procedure Test_NFSE_LimparLista;
    procedure Test_NFSE_CarregarXML;
    procedure Test_NFSE_CarregarINI;
    procedure Test_NFSE_ObterXml;
    procedure Test_NFSE_GravarXml;
    procedure Test_NFSE_ObterIni;
    procedure Test_NFSE_GravarIni;
    procedure Test_NFSE_ObterCertificados;
    procedure Test_NFSE_Emitir;
    procedure Test_NFSE_Cancelar;
    procedure Test_NFSE_SubstituirNFSe;
    procedure Test_NFSE_LinkNFSe;
    procedure Test_NFSE_GerarLote;
    procedure Test_NFSE_GerarToken;
    procedure Test_NFSE_ConsultarSituacao;
    procedure Test_NFSE_ConsultarLoteRps;
    procedure Test_NFSE_ConsultarNFSePorRps;
    procedure Test_NFSE_ConsultarNFSePorNumero;
    procedure Test_NFSE_ConsultarNFSePorPeriodo;
    procedure Test_NFSE_ConsultarNFSePorFaixa;
    procedure Test_NFSE_ConsultarNFSeGenerico;
    procedure Test_NFSE_ConsultarLinkNFSe;
    procedure Test_NFSE_EnviarEmail;
    procedure Test_NFSE_Imprimir;
    procedure Test_NFSE_ImprimirPDF;
    procedure Test_NFSE_ConsultarNFSeServicoPrestadoPorNumero;
    procedure Test_NFSE_ConsultarNFSeServicoPrestadoPorPeriodo;
    procedure Test_NFSE_ConsultarNFSeServicoPrestadoPorTomador;
    procedure Test_NFSE_ConsultarNFSeServicoPrestadoPorIntermediario;
    procedure Test_NFSE_ConsultarNFSeServicoTomadoPorNumero;
    procedure Test_NFSE_ConsultarNFSeServicoTomadoPorPrestador;
    procedure Test_NFSE_ConsultarNFSeServicoTomadoPorTomador;
    procedure Test_NFSE_ConsultarNFSeServicoTomadoPorPeriodo;
    procedure Test_NFSE_ConsultarNFSeServicoTomadoPorIntermediario;
    procedure Test_NFSe_SalvarProvedor;
    procedure Test_NFSe_ImprimirXML;
    procedure Test_NFSe_IniServicos_Vazio_Carregando_ACBrNFSeXServicos_Resources;
    procedure Test_NFSe_IniServicos_Informando_Path_Com_ACBrNFSeXServicos_Sem_Provedor;
    procedure Test_NFSe_IniServicos_Informando_Path_Com_ACBrNFSeXServicos_Com_Provedor;
    procedure Test_NFSe_LayoutImpressao;
    procedure Test_NFSe_ObterInformacoesProvedor;
    procedure Test_NFSe_PropriedadeProducaoNAO;
    procedure Test_NFSe_PropriedadeProducaoSIM;

  end;

implementation

uses
  ACBrLibNFSeStaticImportMT, ACBrLibNFSeConsts, ACBrLibConsts, Dialogs;

procedure TTestACBrNFSeLib.SetUp;
begin
  inherited SetUp;
  fCaminhoExec := ExtractFileDir(ParamStr(0));
end;

procedure TTestACBrNFSeLib.Test_NFSE_Inicializar_Com_DiretorioInvalido;
var
  Handle: THandle;
begin
  try
    //NFSE_Finalizar(Handle);
    AssertEquals(ErrDiretorioNaoExiste, NFSE_Inicializar(Handle,'C:\NAOEXISTE\ACBrLib.ini',''));
  except
  on E: Exception do
     ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end
end;

procedure TTestACBrNFSeLib.Test_NFSE_Inicializar;
var
  Handle: THandle;
begin
    AssertEquals(ErrOk, NFSE_Inicializar(Handle,'',''));
    AssertEquals(ErrOk, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSE_Inicializar_Ja_Inicializado;
var
  Handle: THandle;
begin
    AssertEquals(ErrOk, NFSE_Inicializar(Handle,'',''));
    AssertEquals(ErrOk, NFSE_Inicializar(Handle,'',''));
    AssertEquals(ErrOk, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSE_Finalizar;
var
  Handle: THandle;
begin
    AssertEquals(ErrOk, NFSE_Inicializar(Handle,'',''));
    AssertEquals(ErrOk, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSE_Finalizar_Ja_Finalizado;
var
  Handle: THandle;
begin
    AssertEquals(ErrOk, NFSE_Inicializar(Handle,'',''));
    AssertEquals(ErrOk, NFSE_Finalizar(Handle));
    //AssertEquals(ErrOk, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSE_Nome_Obtendo_LenBuffer;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOk, NFSE_Inicializar(Handle,'',''));

  Bufflen := 0;
  AssertEquals(ErrOk, NFSE_Nome(Handle,Nil, Bufflen));
  AssertEquals(Length(CLibNFSeNome), Bufflen);

  AssertEquals(ErrOk, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSE_Nome_Lendo_Buffer_Tamanho_Identico;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  AssertEquals(ErrOK, NFSE_Inicializar(Handle, '',''));

  Bufflen := Length(CLibNFSeNome);
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, NFSE_Nome(Handle, PChar(AStr), Bufflen));
  AssertEquals(Length(CLibNFSeNome), Bufflen);
  AssertEquals(CLibNFSeNome, AStr);

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSE_Nome_Lendo_Buffer_Tamanho_Maior;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));

  Bufflen := Length(CLibNFSeNome)*2;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, NFSE_Nome(Handle, PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(Length(CLibNFSeNome), Bufflen);
  AssertEquals(CLibNFSeNome, AStr);

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSE_Nome_Lendo_Buffer_Tamanho_Menor;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  AssertEquals(ErrOK, NFSE_Inicializar(Handle,  '', ''));

  Bufflen := 11;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, NFSE_Nome(Handle,  PChar(AStr), Bufflen));
  AssertEquals(11, Bufflen);
  AssertEquals(copy(CLibNFSeNome,1,11), AStr);

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSE_Versao;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOK, NFSE_Inicializar(Handle,  '', ''));

  Bufflen := 0;
  AssertEquals(ErrOk, NFSE_Versao(Handle,  Nil, Bufflen));
  AssertEquals(Length(CLibNFSeVersao), Bufflen);

  // Lendo a resposta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, NFSE_Versao(Handle, PChar(AStr), Bufflen));
  AssertEquals(Length(CLibNFSeVersao), Bufflen);
  AssertEquals(CLibNFSeVersao, AStr);

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSE_ConfigLerValor;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));

  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, NFSE_ConfigLerValor(Handle, CSessaoVersao, CLibNFSeNome, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals(CLibNFSeVersao, AStr);

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSE_ConfigGravarValor;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
    // Gravando o valor
  AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));

  AssertEquals('Erro ao Mudar configuração', ErrOk, NFSE_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, NFSE_ConfigLerValor(Handle, CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '4', AStr);

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSE_LimparLista;
var
  Handle: THandle;
begin
  AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));

  AssertEquals('Erro ao Limpar Lista NFSe', ErrOK, NFSE_LimparLista(Handle));

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSE_CarregarXML;
var
  Handle: THandle;
begin
  AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));

  AssertEquals('Erro ao carregar XML NFSe', ErrOK,
  NFSE_CarregarXML(Handle, PChar(fCaminhoExec+'\NFSe.xml')));

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSE_CarregarINI;
var
  Handle: THandle;
begin
  AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));

  AssertTrue('Handle não iniciado', Handle <> 0);

  AssertEquals('Erro ao carregar o INI NFSe', ErrOK,
  NFSE_CarregarINI(Handle, PChar(fCaminhoExec +'\NFSe.ini')));

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSE_ObterXml;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  try
    AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));
    Resposta:= '';
    Tamanho:= 0;

    AssertEquals('Erro ao ObterXML', ErrOK,
    NFSE_ObterXml(Handle, 0, Resposta, Tamanho));
    AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
    AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

    AssertEquals(ErrOK, NFSE_Finalizar(Handle));
  except
  on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;

end;

procedure TTestACBrNFSeLib.Test_NFSE_GravarXml;
var
  Handle: THandle;
begin
  try
    AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));

    AssertEquals('Erro ao Gravar XML', ErrOK,
    NFSE_GravarXml(Handle, 0, 'NFSeTeste', PChar(fCaminhoExec)));//'C:\ProjetoACBr\ACBr\Projetos\ACBrLib\Testes\NFSe\bin'));

    AssertEquals(ErrOK, NFSE_Finalizar(Handle));
  except
  on E: Exception do
     ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrNFSeLib.Test_NFSE_ObterIni;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  try
    AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));
    Resposta:= '';
    Tamanho:= 0;

    AssertEquals('Erro ao Obter Ini', ErrOK,
    NFSE_ObterIni(Handle, 0, Resposta, Tamanho));
    AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
    AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

    AssertEquals(ErrOK, NFSE_Finalizar(Handle));
  except
  on E: Exception do
     ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrNFSeLib.Test_NFSE_GravarIni;
var
  Handle: THandle;
begin
  try
    AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));

    AssertEquals('Erro ao Gravar Ini', ErrOK,
    NFSE_GravarIni(Handle, 0, 'NFSeTeste', PChar(fCaminhoExec)));//'C:\ProjetoACBr\ACBr\Projetos\ACBrLib\Testes\NFSe\bin'));

    AssertEquals(ErrOK, NFSE_Finalizar(Handle));
  except
  on E: Exception do
     ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrNFSeLib.Test_NFSE_ObterCertificados;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));
  Resposta:= '';
  Tamanho:= 0;

  AssertEquals('Erro ao Obter Certificados', ErrOK,
  NFSE_ObterCertificados(Handle, Resposta, Tamanho));
  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSE_Emitir;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));
  Resposta:= '';
  Tamanho:= 0;

  AssertEquals('Erro ao Limpar Lista NFSe', ErrOK, NFSE_LimparLista(Handle));

  AssertEquals('Erro ao carregar XML NFSe', ErrOK,
  NFSE_CarregarXML(Handle, PChar(fCaminhoExec+'\NFSe.xml')));

  AssertEquals('Erro ao Emitir NFSe', ErrOK,
  NFSE_Emitir(Handle, '1', 0, False, Resposta, Tamanho));
  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSE_Cancelar();
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  try
    AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));
    Resposta:= '';
    Tamanho:= 0;

    AssertEquals('Erro ao Cancelar NFSe', ErrOK,
    NFSE_Cancelar(Handle, PChar(fCaminhoExec +'\NFSeCancelamento.ini'), Resposta, Tamanho));
    AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
    AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

    AssertEquals(ErrOK, NFSE_Finalizar(Handle));

  except
  on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;

end;

procedure TTestACBrNFSeLib.Test_NFSE_SubstituirNFSe();
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  try
    AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));
    Resposta:= '';
    Tamanho:= 0;

    AssertEquals('Erro ao Substituir NFSe', ErrOK,
    NFSE_SubstituirNFSe(Handle,'123', '1', '123', 'TesteSubstituicao', '1', '1', Resposta, Tamanho));
    AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
    AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

    AssertEquals(ErrOK, NFSE_Finalizar(Handle));
  except
  on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrNFSeLib.Test_NFSE_LinkNFSe();
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));
  Resposta:= '';
  Tamanho:= 0;

  AssertEquals('Erro LinkNFSe', ErrOK,
  NFSE_LinkNFSe(Handle, '123', '123', '123', '100', Resposta, Tamanho));
  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSE_GerarLote();
var
  Handle:THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));
  Resposta:= '';
  Tamanho:= 0;

  AssertEquals('Erro ao Gerar Lote', ErrOK,
  NFSE_GerarLote(Handle, '1', 1, 1, Resposta, Tamanho));
  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSE_GerarToken();
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  try
    AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));
    Resposta:= '';
    Tamanho:= 0;

    AssertEquals('Erro ao Gerar Token', ErrOK,
    NFSE_GerarToken(Handle, Resposta, Tamanho));
    AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
    AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

    AssertEquals(ErrOK, NFSE_Finalizar(Handle));
  except
  on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrNFSeLib.Test_NFSE_ConsultarSituacao();
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  try
    AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));
    Resposta:= '';
    Tamanho:= 0;

    AssertEquals('Erro ao Consultar Situação', ErrOK,
    NFSE_ConsultarSituacao(Handle, '123', '123', Resposta, Tamanho));
    AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
    AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

    AssertEquals(ErrOK, NFSE_Finalizar(Handle));
  except
  on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrNFSeLib.Test_NFSE_ConsultarLoteRps();
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  try
    AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));
    Resposta:= '';
    Tamanho:= 0;

    AssertEquals('Erro ao Consultar Lote RPS', ErrOK,
    NFSE_ConsultarLoteRps(Handle, '123', '123', Resposta, Tamanho));
    AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
    AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

    AssertEquals(ErrOK, NFSE_Finalizar(Handle));
  except
  on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrNFSeLib.Test_NFSE_ConsultarNFSePorRps();
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));
  Resposta:= '';
  Tamanho:= 0;

  AssertEquals('Erro ao Consultar NFSe Por RPS', ErrOK,
  NFSE_ConsultarNFSePorRps(Handle, '123', '1', '1', '123', Resposta, Tamanho));
  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSE_ConsultarNFSePorNumero();
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  try
    AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));
    Resposta:= '';
    Tamanho:= 0;

    AssertEquals('Erro ao Consultar NFSe Por Numero', ErrOK,
    NFSE_ConsultarNFSePorNumero(Handle, '1', 1, Resposta, Tamanho));
    AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
    AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

    AssertEquals(ErrOK, NFSE_Finalizar(Handle));
  except
  on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrNFSeLib.Test_NFSE_ConsultarNFSePorPeriodo();
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
  dataInicial, dataFinal: TDateTime;
begin
  try
    AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));
    Resposta:= '';
    Tamanho:= 0;
    dataInicial:= EncodeDate(2023, 04, 10);
    dataFinal:= EncodeDate(2023, 04, 11);

    AssertEquals('Erro ao Consultar NFSe Por Periodo', ErrOK,
    NFSE_ConsultarNFSePorPeriodo(Handle, dataInicial, dataFinal, 1, '1', 1, Resposta, Tamanho));
    AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
    AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

    AssertEquals(ErrOK, NFSE_Finalizar(Handle));
  except
  on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrNFSeLib.Test_NFSE_ConsultarNFSePorFaixa();
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  try
    AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));
    Resposta:= '';
    Tamanho:= 0;

    AssertEquals('Erro ao Consultar NFSe Por Faixa', ErrOK,
    NFSE_ConsultarNFSePorFaixa(Handle, '1', '2', 1, Resposta, Tamanho));
    AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
    AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

    AssertEquals(ErrOK, NFSE_Finalizar(Handle));
  except
  on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrNFSeLib.Test_NFSE_ConsultarNFSeGenerico();
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  try
    AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));
    Resposta:= '';
    Tamanho:= 0;

    AssertEquals('Erro ao Consultar NFSe Generico', ErrOK,
    NFSE_ConsultarNFSeGenerico(Handle, PChar(fCaminhoExec +'\NFSeConsulta.ini'), Resposta, Tamanho));
    AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
    AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

    AssertEquals(ErrOK, NFSE_Finalizar(Handle));
  except
  on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrNFSeLib.Test_NFSE_ConsultarLinkNFSe();
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  try
    AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));
    Resposta:= '';
    Tamanho:= 0;

    AssertEquals('Erro ao Consultar Link NFSe', ErrOK,
    NFSE_ConsultarLinkNFSe(Handle, PChar(fCaminhoExec +'\NFSeConsultaLinkNFse.ini'), Resposta, Tamanho));
    AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
    AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

    AssertEquals(ErrOK, NFSE_Finalizar(Handle));
  except
  on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrNFSeLib.Test_NFSE_EnviarEmail();
var
  Handle: THandle;
  Path, ArqNFSe: String;
begin
  try
    AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));
    Path:= 'C:\ProjetoACBr\ACBr\Projetos\ACBrLib\Testes\NFSe\bin\';
    ArqNFSe:= Path + 'NFSe.xml';

    AssertEquals('Erro ao Enviar E-mail', ErrOK,
    NFSE_EnviarEmail(Handle, 'nome@provedor.com', PChar(ArqNFSe), True, 'Teste', '', '', 'Em anexo o NFSe'));

    AssertEquals(ErrOK, NFSE_Finalizar(Handle));
  except
  on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrNFSeLib.Test_NFSE_Imprimir();
var
  Handle: THandle;
begin
  AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));

  AssertEquals('Erro ao Imprimir NFSe', ErrOK,
  NFSE_Imprimir(Handle, '', 1, '', '', ''));

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSE_ImprimirPDF();
var
  Handle: THandle;
begin
  AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));

  AssertEquals('Erro ao Imprimir PDF NFSe', ErrOK,
  NFSE_ImprimirPDF(Handle));

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSE_ConsultarNFSeServicoPrestadoPorNumero();
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
  dataInicial, dataFinal: TDateTime;
begin
  try
     AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));
     Resposta:= '';
     Tamanho:= 0;
     dataInicial:= EncodeDate(2023, 04, 10);
     dataFinal:= EncodeDate(2023, 04, 11);

     AssertEquals('Erro ao Consultar NFSe Serviço Prestado Por Numero', ErrOK,
     NFSE_ConsultarNFSeServicoPrestadoPorNumero(Handle,'1', 1, dataInicial, dataFinal, 1, Resposta, Tamanho));
     AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
     AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

     AssertEquals(ErrOK, NFSE_Finalizar(Handle));
  except
  on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrNFSeLib.Test_NFSE_ConsultarNFSeServicoPrestadoPorPeriodo();
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
  dataInicial, dataFinal: TDateTime;
begin
  try
     AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));
     Resposta:= '';
     Tamanho:= 0;
     dataInicial:= EncodeDate(2023, 04, 10);
     dataFinal:= EncodeDate(2023, 04, 11);

     AssertEquals('Erro ao Consultar NFSe Serviço Prestado Por Periodo', ErrOK,
     NFSE_ConsultarNFSeServicoPrestadoPorPeriodo(Handle, dataInicial, dataFinal, 1, 1, Resposta, Tamanho));
     AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
     AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

     AssertEquals(ErrOK, NFSE_Finalizar(Handle));
  except
  on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrNFSeLib.Test_NFSE_ConsultarNFSeServicoPrestadoPorTomador();
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
  dataInicial, dataFinal: TDateTime;
begin
  try
     AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));
     Resposta:= '';
     Tamanho:= 0;
     dataInicial:= EncodeDate(2023, 04, 10);
     dataFinal:= EncodeDate(2023, 04, 11);

     AssertEquals('Erro ao Consultar NFSe Serviço Prestado Por Tomador', ErrOK,
     NFSE_ConsultarNFSeServicoPrestadoPorTomador(Handle, '123', '111', 1, dataInicial, dataFinal, 1, Resposta, Tamanho));
     AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
     AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

     AssertEquals(ErrOK, NFSE_Finalizar(Handle));
  except
  on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrNFSeLib.Test_NFSE_ConsultarNFSeServicoPrestadoPorIntermediario();
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
  dataInicial, dataFinal: TDateTime;
begin
  try
     AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));
     Resposta:= '';
     Tamanho:= 0;
     dataInicial:= EncodeDate(2023, 04, 10);
     dataFinal:= EncodeDate(2023, 04, 11);

     AssertEquals('Erro ao Consultar NFSe Serviço Prestado Por Intermediario',ErrOK,
     NFSE_ConsultarNFSeServicoPrestadoPorIntermediario(Handle, '123', '123', 1, dataInicial, dataFinal, 1, Resposta, Tamanho));
     AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
     AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

     AssertEquals(ErrOK, NFSE_Finalizar(Handle));
  except
  on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrNFSeLib.Test_NFSE_ConsultarNFSeServicoTomadoPorNumero();
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
  dataInicial, dataFinal: TDateTime;
begin
  try
     AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));
     Resposta:= '';
     Tamanho:= 0;
     dataInicial:= EncodeDate(2023, 04, 10);
     dataFinal:= EncodeDate(2023, 04, 11);

     AssertEquals('Erro ao Consultar NFSe Serviço Tomado Por Numero',ErrOK,
     NFSE_ConsultarNFSeServicoTomadoPorNumero(Handle, '1', 1, dataInicial, dataFinal, 1, Resposta, Tamanho));
     AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
     AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

     AssertEquals(ErrOK, NFSE_Finalizar(Handle));
  except
  on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrNFSeLib.Test_NFSE_ConsultarNFSeServicoTomadoPorPrestador();
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
  dataInicial, dataFinal: TDateTime;
begin
  try
     AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));
     Resposta:= '';
     Tamanho:= 0;
     dataInicial:= EncodeDate(2023, 04, 10);
     dataFinal:= EncodeDate(2023, 04, 11);

     AssertEquals('Erro ao Consultar NFSe Serviço Tomado Por Prestador',ErrOK,
     NFSE_ConsultarNFSeServicoTomadoPorPrestador(Handle, '123', '123', 1, dataInicial, dataFinal, 1, Resposta, Tamanho));
     AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
     AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

     AssertEquals(ErrOK, NFSE_Finalizar(Handle));
  except
  on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrNFSeLib.Test_NFSE_ConsultarNFSeServicoTomadoPorTomador();
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
  dataInicial, dataFinal: TDateTime;
begin
  try
     AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));
     Resposta:= '';
     Tamanho:= 0;
     dataInicial:= EncodeDate(2023, 04, 10);
     dataFinal:= EncodeDate(2023, 04, 11);

     AssertEquals('Erro ao Consultar NFSe Serviço Tomado Por Tomador',ErrOK,
     NFSE_ConsultarNFSeServicoTomadoPorTomador(Handle, '123', '123', 1, dataInicial, dataFinal, 1, Resposta, Tamanho));
     AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
     AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

     AssertEquals(ErrOK, NFSE_Finalizar(Handle));
  except
  on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrNFSeLib.Test_NFSE_ConsultarNFSeServicoTomadoPorPeriodo();
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
  dataInicial, dataFinal: TDateTime;
begin
  try
     AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));
     Resposta:= '';
     Tamanho:= 0;
     dataInicial:= EncodeDate(2023, 04, 10);
     dataFinal:= EncodeDate(2023, 04, 11);

     AssertEquals('Erro ao Consultar NFSe Serviço Tomado Por Periodo',ErrOK,
     NFSE_ConsultarNFSeServicoTomadoPorPeriodo(Handle, dataInicial, dataFinal, 1, 1, Resposta, Tamanho));
     AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
     AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

     AssertEquals(ErrOK, NFSE_Finalizar(Handle));
  except
  on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrNFSeLib.Test_NFSE_ConsultarNFSeServicoTomadoPorIntermediario();
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
  dataInicial, dataFinal: TDateTime;
begin
  try
     AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));
     Resposta:= '';
     Tamanho:= 0;
     dataInicial:= EncodeDate(2023, 04, 10);
     dataFinal:= EncodeDate(2023, 04, 11);

     AssertEquals('Erro ao Consultar NFSe Serviço Tomado Por Intermediario',ErrOK,
     NFSE_ConsultarNFSeServicoTomadoPorIntermediario(Handle, '123', '123', 1, dataInicial, dataFinal, 1, Resposta, Tamanho));
     AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
     AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

     AssertEquals(ErrOK, NFSE_Finalizar(Handle));
  except
  on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrNFSeLib.Test_NFSe_SalvarProvedor();
var
  AStr: String;
  Handle: THandle;
  Bufflen: Integer;
begin
  AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));

  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, NFSE_ConfigLerValor(Handle, 'NFSe', 'CodigoMunicipio', PChar(AStr), Bufflen));

  AssertEquals('Erro ao Mudar configuração', ErrOk, NFSE_ConfigGravarValor(Handle, 'NFSe', 'CodigoMunicipio', '5106224'));

  AssertEquals(ErrOK, NFSE_ConfigGravar(Handle,'ACBrLib.ini'));

  AssertEquals('Erro ao Mudar configuração', ErrOk, NFSE_ConfigGravarValor(Handle, 'NFSe', 'CodigoMunicipio', '3501608'));

  AssertEquals(ErrOK, NFSE_ConfigGravar(Handle,'ACBrLib.ini'));

  AStr := copy(AStr,1,Bufflen);


  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSe_ImprimirXML();
var
  AStr: String;
  Handle: THandle;
  Bufflen: Integer;
begin
  AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));

  AssertEquals(ErrOk, NFSE_ConfigLerValor(Handle, 'NFSe', 'CodigoMunicipio', PChar(AStr), Bufflen));

  AssertEquals('Erro ao Mudar configuração', ErrOk, NFSE_ConfigGravarValor(Handle, 'NFSe', 'CodigoMunicipio', '3506003'));

  AssertEquals(ErrOK, NFSE_ConfigGravar(Handle,'ACBrLib.ini'));

  AssertEquals('Erro ao carregar XML NFSe', ErrOK,
  NFSE_CarregarXML(Handle, PChar(fCaminhoExec+'\540-nfse.xml')));

  AssertEquals('Erro ao Imprimir NFSe', ErrOK,
  NFSE_Imprimir(Handle, '', 1, '', '1', ''));

  AssertEquals('Erro ao Limpar Lista NFSe', ErrOK, NFSE_LimparLista(Handle));

  AssertEquals('Erro ao carregar XML NFSe', ErrOK,
  NFSE_CarregarXML(Handle, PChar(fCaminhoExec+'\540-nfse.xml')));

  AssertEquals('Erro ao Imprimir NFSe', ErrOK,
  NFSE_Imprimir(Handle, '', 1, '', '1', ''));

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSe_IniServicos_Vazio_Carregando_ACBrNFSeXServicos_Resources();
var
  AStr: String;
  Handle: THandle;
  Bufflen: Integer;
  Resposta: PChar;
  Tamanho: Longint;
begin
  AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));
  Resposta:= '';
  Tamanho:= 0;

    AssertEquals(ErrOk, NFSE_ConfigLerValor(Handle, 'NFSe', 'IniServicos', PChar(AStr), Bufflen));

  AssertEquals('Erro ao Mudar configuração', ErrOk, NFSE_ConfigGravarValor(Handle, 'NFSe', 'IniServicos', ''));

  AssertEquals('Erro ao Mudar configuração', ErrOk, NFSE_ConfigGravarValor(Handle, 'NFSe', 'CodigoMunicipio', '3554003'));

  AssertEquals(ErrOk, NFSE_ConfigLerValor(Handle, 'NFSe', 'CodigoMunicipio', PChar(AStr), Bufflen));

  AssertEquals(ErrOK, NFSE_ConfigGravar(Handle,'ACBrLib.ini'));

  AssertEquals('Erro ao carregar XML NFSe', ErrOK,
  NFSE_CarregarINI(Handle, PChar(fCaminhoExec+'\IniNFSE-UmServico.ini')));

  AssertEquals('Erro ao Emitir NFSe', ErrOK,
  NFSE_Emitir(Handle, '1', 0, False, Resposta, Tamanho));
  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSe_IniServicos_Informando_Path_Com_ACBrNFSeXServicos_Sem_Provedor();
var
  AStr: String;
  Handle: THandle;
  Bufflen: Integer;
  Resposta: PChar;
  Tamanho: Longint;
begin
  AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));
  Resposta:= '';
  Tamanho:= 0;

  AssertEquals(ErrOk, NFSE_ConfigLerValor(Handle, 'NFSe', 'IniServicos', PChar(AStr), Bufflen));

  AssertEquals('Erro ao Mudar configuração', ErrOk, NFSE_ConfigGravarValor(Handle, 'NFSe', 'IniServicos', 'C:\ProjetoACBr\ACBrLib\ACBrLibNFSe-1.0.0.20\dep\ACBrNFSeXServicos.ini'));

  AssertEquals('Erro ao Mudar configuração', ErrOk, NFSE_ConfigGravarValor(Handle, 'NFSe', 'CodigoMunicipio', '3554003'));

  AssertEquals(ErrOk, NFSE_ConfigLerValor(Handle, 'NFSe', 'CodigoMunicipio', PChar(AStr), Bufflen));

  AssertEquals(ErrOK, NFSE_ConfigGravar(Handle,'ACBrLib.ini'));

  AssertEquals('Erro ao carregar XML NFSe', ErrOK,
  NFSE_CarregarINI(Handle, PChar(fCaminhoExec+'\IniNFSE-UmServico.ini')));

  AssertEquals('Erro ao Emitir NFSe', ErrOK,
  NFSE_Emitir(Handle, '1', 0, False, Resposta, Tamanho));
  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSe_IniServicos_Informando_Path_Com_ACBrNFSeXServicos_Com_Provedor();
var
  AStr: String;
  Handle: THandle;
  Bufflen: Integer;
  Resposta: PChar;
  Tamanho: Longint;
begin
  AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));
  Resposta:= '';
  Tamanho:= 0;

  AssertEquals(ErrOk, NFSE_ConfigLerValor(Handle, 'NFSe', 'IniServicos', PChar(AStr), Bufflen));

  AssertEquals('Erro ao Mudar configuração', ErrOk, NFSE_ConfigGravarValor(Handle, 'NFSe', 'IniServicos', 'C:\ProjetoACBr\ACBrLib\ACBrLibNFSe-1.0.0.24\dep\ACBrNFSeXServicos.ini'));

  AssertEquals('Erro ao Mudar configuração', ErrOk, NFSE_ConfigGravarValor(Handle, 'NFSe', 'CodigoMunicipio', '3554003'));

  AssertEquals(ErrOk, NFSE_ConfigLerValor(Handle, 'NFSe', 'CodigoMunicipio', PChar(AStr), Bufflen));

  AssertEquals(ErrOK, NFSE_ConfigGravar(Handle,'ACBrLib.ini'));

  AssertEquals('Erro ao carregar XML NFSe', ErrOK,
  NFSE_CarregarINI(Handle, PChar(fCaminhoExec+'\IniNFSE-UmServico.ini')));

  AssertEquals('Erro ao Emitir NFSe', ErrOK,
  NFSE_Emitir(Handle, '1', 0, False, Resposta, Tamanho));
  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSe_LayoutImpressao;
var
  AStr: String;
  Handle: THandle;
  Bufflen: Integer;
begin
  AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));

  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals(ErrOk, NFSE_ConfigLerValor(Handle, 'NFSe', 'LayoutNFSe', PChar(AStr), Bufflen));

  AssertEquals('Erro ao Mudar configuração', ErrOk, NFSE_ConfigGravarValor(Handle, 'NFSe', 'LayoutNFSe', '0'));

  AssertEquals(ErrOk, NFSE_ConfigLerValor(Handle, 'NFSe', 'CodigoMunicipio', PChar(AStr), Bufflen));

  AssertEquals('Erro ao Mudar configuração', ErrOk, NFSE_ConfigGravarValor(Handle, 'NFSe', 'CodigoMunicipio', '3301702'));

  AssertEquals(ErrOK, NFSE_ConfigGravar(Handle,'ACBrLib.ini'));

  AssertEquals('Erro ao carregar XML NFSe', ErrOK,
  NFSE_CarregarXML(Handle, PChar(fCaminhoExec+'\NFSe.xml')));

  AssertEquals('Erro ao Imprimir NFSe', ErrOK,
  NFSE_Imprimir(Handle, '', 1, 'True', 'True', ''));

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSe_ObterInformacoesProvedor;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: longint;
begin

  AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));
  Resposta := '';
  Tamanho := 0;

  NFSE_ObterInformacoesProvedor(Handle, Resposta, Tamanho);

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

  AssertEquals('Erro ao configurar a cidade', ErrOK,
  NFSE_ConfigGravarValor(Handle, 'NFSe', 'LayoutNFSe', '1'));

  Resposta := '';
  Tamanho := 0;
  NFSE_ObterInformacoesProvedor(Handle, Resposta, Tamanho);

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSe_PropriedadeProducaoNAO;
var
  AStr: String;
  Handle: THandle;
  Bufflen: Integer;
begin
  AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));

  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals(ErrOk, NFSE_ConfigLerValor(Handle, 'DANFSE', 'Producao', PChar(AStr), Bufflen));

  AssertEquals('Erro ao Mudar configuração', ErrOk, NFSE_ConfigGravarValor(Handle, 'DANFSE', 'Producao', '0')); //Deve exibir Tarja "Ambiente de Homologação".

  AssertEquals(ErrOk, NFSE_ConfigLerValor(Handle, 'NFSe', 'CodigoMunicipio', PChar(AStr), Bufflen));

  AssertEquals('Erro ao Mudar configuração', ErrOk, NFSE_ConfigGravarValor(Handle, 'NFSe', 'CodigoMunicipio', '4321204'));

  AssertEquals(ErrOK, NFSE_ConfigGravar(Handle,'ACBrLib.ini'));

  AssertEquals('Erro ao carregar XML NFSe', ErrOK,
  NFSE_CarregarXML(Handle, PChar(fCaminhoExec+'\TAQUARA.xml')));

  AssertEquals('Erro ao Imprimir NFSe', ErrOK,
  NFSE_Imprimir(Handle, '', 1, 'True', 'True', ''));

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSe_PropriedadeProducaoSIM;
var
  AStr: String;
  Handle: THandle;
  Bufflen: Integer;
begin
  AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));

  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals(ErrOk, NFSE_ConfigLerValor(Handle, 'DANFSE', 'Producao', PChar(AStr), Bufflen));

  AssertEquals('Erro ao Mudar configuração', ErrOk, NFSE_ConfigGravarValor(Handle, 'DANFSE', 'Producao', '1')); //Não deve exibir Tarja "Ambiente de Homologação"

  AssertEquals(ErrOk, NFSE_ConfigLerValor(Handle, 'NFSe', 'CodigoMunicipio', PChar(AStr), Bufflen));

  AssertEquals('Erro ao Mudar configuração', ErrOk, NFSE_ConfigGravarValor(Handle, 'NFSe', 'CodigoMunicipio', '4321204'));

  AssertEquals(ErrOK, NFSE_ConfigGravar(Handle,'ACBrLib.ini'));

  AssertEquals('Erro ao carregar XML NFSe', ErrOK,
  NFSE_CarregarXML(Handle, PChar(fCaminhoExec+'\TAQUARA.xml')));

  AssertEquals('Erro ao Imprimir NFSe', ErrOK,
  NFSE_Imprimir(Handle, '', 1, 'True', 'True', ''));

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

initialization

  RegisterTest(TTestACBrNFSeLib);
end.

