{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2013 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  André Ferreira Moraes                          }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrSATMFe_integrador ;

interface

uses
  Classes, SysUtils, ACBrSATClass, pcnGerador, ACBrIntegradorUtil,
  pcnVFPe;

const
  cNomeComponente = 'MF-e';

type

   { TACBrSATMFe_integrador_XML }

   TACBrSATMFe_integrador_XML = class( TACBrSATClass )
   private
     FGerador: TGerador;
     FComandoMFE: TComandoIntegrador;
     FIdentificador: TIdentificador;
     FParametro: TParametro;
     FMetodo: TMetodo;

     FPastaInput : String;
     FPastaOutput : String;
     FTimeout : Integer;
     procedure SetPastaInput(AValue: String);
     procedure SetPastaOutput(AValue: String);
     procedure SetTimeout(AValue: Integer);
   protected
     procedure LoadDLLFunctions ; override;
     procedure UnLoadDLLFunctions; override;

   public
     constructor Create( AOwner : TComponent ) ; override;
     destructor Destroy; override;

     function AssociarAssinatura( CNPJvalue, assinaturaCNPJs : AnsiString ):
       String ; override;
     function AtivarSAT( subComando : Integer; CNPJ: AnsiString; cUF : Integer )
       : String ; override;
     function AtualizarSoftwareSAT : String ; override;
     function BloquearSAT : String ; override;
     function CancelarUltimaVenda( chave, dadosCancelamento : AnsiString ) :
       String ; override;
     function ComunicarCertificadoICPBRASIL( certificado : AnsiString ) :
       String ; override;
     function ConfigurarInterfaceDeRede( dadosConfiguracao : AnsiString ) :
       String ; override;
     function ConsultarNumeroSessao( cNumeroDeSessao : Integer) : String ;
       override;
     function ConsultarSAT : String ; override ;
     function ConsultarStatusOperacional : String ; override;
     function DesbloquearSAT : String ; override;
     function EnviarDadosVenda( dadosVenda : AnsiString ) : String ; override;
     function ExtrairLogs : String ; override;
     function TesteFimAFim( dadosVenda : AnsiString) : String ; override;
     function TrocarCodigoDeAtivacao( codigoDeAtivacaoOuEmergencia: AnsiString;
       opcao : Integer; novoCodigo: AnsiString ) : String ; override;

     function EnviarPagamento( Pagamento : TEnviarPagamento ): TRespostaPagamento;
     function VerificarStatusValidador( VerificarStatusValidador : TVerificarStatusValidador ): TRespostaVerificarStatusValidador;
     function RespostaFiscal( RespostaFiscal : TRespostaFiscal ): TRetornoRespostaFiscal;
     function EnviarStatusPagamento( StatusPagamento : TStatusPagamento ): TRespostaStatusPagamento;

   published
     property PastaInput  : String  read FPastaInput  write SetPastaInput;
     property PastaOutput : String  read FPastaOutput write SetPastaOutput;
     property Timeout     : Integer read FTimeout     write SetTimeout default 30;
   end;

implementation

Uses ACBrUtil, pcnConversao, dateutils, ACBrSAT;

procedure TACBrSATMFe_integrador_XML.SetPastaInput(AValue: String);
begin
  if FPastaInput=AValue then
    Exit;
  FPastaInput := AValue;
  FComandoMFE.PastaInput := FPastaInput;
end;

procedure TACBrSATMFe_integrador_XML.SetPastaOutput(AValue: String);
begin
  if FPastaOutput=AValue then
    Exit;
  FPastaOutput := AValue;
  FComandoMFE.PastaOutput := FPastaOutput;
end;

procedure TACBrSATMFe_integrador_XML.SetTimeout(AValue: Integer);
begin
  if FTimeout=AValue then
    Exit;
  FTimeout := AValue;
  FComandoMFE.Timeout := FTimeout;
end;

procedure TACBrSATMFe_integrador_XML.LoadDLLFunctions;
begin
  //Não faz nada
end;

procedure TACBrSATMFe_integrador_XML.UnLoadDLLFunctions;
begin
  //Não faz nada
end;

constructor TACBrSATMFe_integrador_XML.Create(AOwner : TComponent) ;
begin
  inherited Create(AOwner) ;

  fpModeloStr := 'MFe_Integrador_XML' ;
  FGerador       := TGerador.Create;
  FComandoMFE    := TComandoIntegrador.Create;
  FIdentificador := TIdentificador.Create(FGerador);
  FParametro     := TParametro.Create(FGerador);
  FMetodo        := TMetodo.Create(FGerador);

  FPastaInput  := 'C:\Integrador\Input\';
  FPastaOutput := 'C:\Integrador\Output\';
  FTimeout     := 30;

  FComandoMFE.PastaInput := FPastaInput;
  FComandoMFE.PastaOutput := FPastaOutput;
  FComandoMFE.Timeout     := FTimeout;
end ;

destructor TACBrSATMFe_integrador_XML.Destroy;
begin
  FIdentificador.Free;
  FParametro.Free;
  FMetodo.Free;
  FComandoMFE.Free;
  FGerador.Free;
  inherited Destroy;
end;

function TACBrSATMFe_integrador_XML.AssociarAssinatura(CNPJvalue,
  assinaturaCNPJs : AnsiString) : String ;
Var
  Resp : String;
begin
  FGerador.LayoutArquivoTXT.Clear;

  FGerador.ArquivoFormatoXML := '';
  FGerador.ArquivoFormatoTXT := '';

  FMetodo.GerarMetodo(numeroSessao,cNomeComponente,'AssociarAssinatura');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('codigoDeAtivacao',codigoDeAtivacao,tcStr);
  FParametro.GerarParametro('cnpjValue',CNPJvalue,tcStr);
  FParametro.GerarParametro('assinaturaCNPJs',assinaturaCNPJs,tcStr);
  FMetodo.FinalizarMetodo;

  Resp := FComandoMFE.EnviaComando(numeroSessao,'AssociarAssinatura',FGerador.ArquivoFormatoXML);

  Result := FComandoMFE.PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.AtivarSAT(subComando : Integer ;
  CNPJ : AnsiString; cUF : Integer) : String ;
Var
  Resp : String;
begin
  FGerador.LayoutArquivoTXT.Clear;

  FGerador.ArquivoFormatoXML := '';
  FGerador.ArquivoFormatoTXT := '';

  FMetodo.GerarMetodo(numeroSessao,cNomeComponente,'AtivarMFe');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('subComando',subComando,tcInt);
  FParametro.GerarParametro('codigoDeAtivacao',codigoDeAtivacao,tcStr);
  FParametro.GerarParametro('CNPJ',CNPJ,tcStr);
  FParametro.GerarParametro('cUF',cUF,tcInt);
  FMetodo.FinalizarMetodo;

  Resp := FComandoMFE.EnviaComando(numeroSessao,'AtivarMFe',FGerador.ArquivoFormatoXML);

  Result := FComandoMFE.PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.AtualizarSoftwareSAT : String ;
Var
  Resp : String;
begin
  FGerador.LayoutArquivoTXT.Clear;

  FGerador.ArquivoFormatoXML := '';
  FGerador.ArquivoFormatoTXT := '';

  FMetodo.GerarMetodo(numeroSessao,cNomeComponente,'AtualizarSoftwareMFe');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('codigoDeAtivacao',codigoDeAtivacao,tcStr);
  FMetodo.FinalizarMetodo;

  Resp := FComandoMFE.EnviaComando(numeroSessao,'AtualizarSoftwareMFe',FGerador.ArquivoFormatoXML);

  Result := FComandoMFE.PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.BloquearSAT : String ;
Var
  Resp : String;
begin
  FGerador.LayoutArquivoTXT.Clear;

  FGerador.ArquivoFormatoXML := '';
  FGerador.ArquivoFormatoTXT := '';

  FMetodo.GerarMetodo(numeroSessao,cNomeComponente,'BloquearMFe');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('codigoDeAtivacao',codigoDeAtivacao,tcStr);
  FMetodo.FinalizarMetodo;

  Resp := FComandoMFE.EnviaComando(numeroSessao,'BloquearMFe',FGerador.ArquivoFormatoXML);

  Result := FComandoMFE.PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.CancelarUltimaVenda(chave,
  dadosCancelamento : AnsiString) : String ;
Var
  Resp : String;
begin
  FGerador.LayoutArquivoTXT.Clear;

  FGerador.ArquivoFormatoXML := '';
  FGerador.ArquivoFormatoTXT := '';

  FMetodo.GerarMetodo(numeroSessao,cNomeComponente,'CancelarUltimaVenda');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('codigoDeAtivacao',codigoDeAtivacao,tcStr);
  FParametro.GerarParametro('chave',OnlyNumber(chave),tcStr);
  FParametro.GerarParametro('dadosCancelamento','<![CDATA[' +FComandoMFE.AjustaComando(dadosCancelamento)+ ']]>',tcStr, False);
  FMetodo.FinalizarMetodo;

  Resp := FComandoMFE.EnviaComando(numeroSessao,'CancelarUltimaVenda',FGerador.ArquivoFormatoXML);

  Result := FComandoMFE.PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.ComunicarCertificadoICPBRASIL(
  certificado : AnsiString) : String ;
Var
  Resp : String;
begin
  FGerador.LayoutArquivoTXT.Clear;

  FGerador.ArquivoFormatoXML := '';
  FGerador.ArquivoFormatoTXT := '';

  FMetodo.GerarMetodo(numeroSessao,cNomeComponente,'ComunicarCertificadoICPBRASIL');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('codigoDeAtivacao',codigoDeAtivacao,tcStr);
  FParametro.GerarParametro('certificado',certificado,tcStr);
  FMetodo.FinalizarMetodo;

  Resp := FComandoMFE.EnviaComando(numeroSessao,'ComunicarCertificadoICPBRASIL',FGerador.ArquivoFormatoXML);
  Result := FComandoMFE.PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.ConfigurarInterfaceDeRede(
  dadosConfiguracao : AnsiString) : String ;
Var
  Resp : String;
begin
  FGerador.LayoutArquivoTXT.Clear;

  FGerador.ArquivoFormatoXML := '';
  FGerador.ArquivoFormatoTXT := '';

  FMetodo.GerarMetodo(numeroSessao,cNomeComponente,'ConfigurarInterfaceDeRede');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('codigoDeAtivacao',codigoDeAtivacao,tcStr);
  FParametro.GerarParametro('dadosConfiguracao',dadosConfiguracao,tcStr);
  FMetodo.FinalizarMetodo;

  Resp := FComandoMFE.EnviaComando(numeroSessao,'ConfigurarInterfaceDeRede',FGerador.ArquivoFormatoXML);

  Result := FComandoMFE.PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.ConsultarNumeroSessao(cNumeroDeSessao : Integer
  ) : String ;
Var
  Resp : String;
begin
  FGerador.LayoutArquivoTXT.Clear;

  FGerador.ArquivoFormatoXML := '';
  FGerador.ArquivoFormatoTXT := '';

  FMetodo.GerarMetodo(numeroSessao,cNomeComponente,'ConsultarNumeroSessao');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('codigoDeAtivacao',codigoDeAtivacao,tcStr);
  FParametro.GerarParametro('cNumeroDeSessao',cNumeroDeSessao,tcInt);
  FMetodo.FinalizarMetodo;

  Resp := FComandoMFE.EnviaComando(numeroSessao,'ConsultarNumeroSessao',FGerador.ArquivoFormatoXML);

  Result := FComandoMFE.PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.ConsultarSAT : String ;
Var
  Resp : String;
begin
  FGerador.LayoutArquivoTXT.Clear;

  FGerador.ArquivoFormatoXML := '';
  FGerador.ArquivoFormatoTXT := '';

  FMetodo.GerarMetodo(numeroSessao,cNomeComponente,'ConsultarMFe');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FMetodo.FinalizarMetodo;

  Resp := FComandoMFE.EnviaComando(numeroSessao,'ConsultarMFe',FGerador.ArquivoFormatoXML);

  Result := FComandoMFE.PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.ConsultarStatusOperacional : String ;
Var
  Resp : String;
begin
  FGerador.LayoutArquivoTXT.Clear;

  FGerador.ArquivoFormatoXML := '';
  FGerador.ArquivoFormatoTXT := '';

  FMetodo.GerarMetodo(numeroSessao,cNomeComponente,'ConsultarStatusOperacional');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('codigoDeAtivacao',codigoDeAtivacao,tcStr);
  FMetodo.FinalizarMetodo;

  Resp := FComandoMFE.EnviaComando(numeroSessao,'ConsultarStatusOperacional',FGerador.ArquivoFormatoXML);

  Result := FComandoMFE.PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.DesbloquearSAT : String ;
Var
  Resp : String;
begin
  FGerador.LayoutArquivoTXT.Clear;

  FGerador.ArquivoFormatoXML := '';
  FGerador.ArquivoFormatoTXT := '';

  FMetodo.GerarMetodo(numeroSessao,cNomeComponente,'DesbloquearMFe');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('codigoDeAtivacao',codigoDeAtivacao,tcStr);
  FMetodo.FinalizarMetodo;

  Resp := FComandoMFE.EnviaComando(numeroSessao,'DesbloquearMFe',FGerador.ArquivoFormatoXML);

  Result := FComandoMFE.PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.EnviarDadosVenda(dadosVenda : AnsiString) : String ;
Var
  Resp : String;
begin
  FGerador.LayoutArquivoTXT.Clear;

  FGerador.ArquivoFormatoXML := '';
  FGerador.ArquivoFormatoTXT := '';

  FMetodo.GerarMetodo(numeroSessao,cNomeComponente,'EnviarDadosVenda');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('codigoDeAtivacao',codigoDeAtivacao,tcStr);
  FParametro.GerarParametro('dadosVenda','<![CDATA[' +FComandoMFE.AjustaComando(dadosVenda)+ ']]>',tcStr, False);
  FParametro.GerarParametro('nrDocumento',numeroSessao,tcInt);
  FMetodo.FinalizarMetodo;

  //if FComandoMFE.ErroTimeout then
  //  ConsultarSAT;

  Resp := FComandoMFE.EnviaComando(numeroSessao,'EnviarDadosVenda',FGerador.ArquivoFormatoXML);
  Result := FComandoMFE.PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.ExtrairLogs : String ;
Var
  Resp : String;
begin
  FGerador.LayoutArquivoTXT.Clear;

  FGerador.ArquivoFormatoXML := '';
  FGerador.ArquivoFormatoTXT := '';

  FMetodo.GerarMetodo(numeroSessao,cNomeComponente,'ExtrairLogs');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('codigoDeAtivacao',codigoDeAtivacao,tcStr);
  FMetodo.FinalizarMetodo;

  Resp := FComandoMFE.EnviaComando(numeroSessao,'ExtrairLogs',FGerador.ArquivoFormatoXML);
  Result := FComandoMFE.PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.TesteFimAFim(dadosVenda : AnsiString) : String ;
Var
  Resp : String;
begin
  FGerador.LayoutArquivoTXT.Clear;

  FGerador.ArquivoFormatoXML := '';
  FGerador.ArquivoFormatoTXT := '';

  FMetodo.GerarMetodo(numeroSessao,cNomeComponente,'TesteFimAFim');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('codigoDeAtivacao',codigoDeAtivacao,tcStr);
  FParametro.GerarParametro('dadosVenda','<![CDATA[' +FComandoMFE.AjustaComando(dadosVenda)+ ']]>',tcStr, False);
 // FParametro.GerarParametro('nrDocumento',numeroSessao,tcInt);
  FMetodo.FinalizarMetodo;

  Resp := FComandoMFE.EnviaComando(numeroSessao,'TesteFimAFim',FGerador.ArquivoFormatoXML);

  Result := FComandoMFE.PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.TrocarCodigoDeAtivacao(
  codigoDeAtivacaoOuEmergencia: AnsiString; opcao: Integer; novoCodigo: AnsiString
  ): String;
Var
  Resp : String;
begin
  if codigoDeAtivacaoOuEmergencia = '' then
    codigoDeAtivacaoOuEmergencia := codigoDeAtivacao;

  FGerador.LayoutArquivoTXT.Clear;

  FGerador.ArquivoFormatoXML := '';
  FGerador.ArquivoFormatoTXT := '';

  FMetodo.GerarMetodo(numeroSessao,cNomeComponente,'TrocarCodigoDeAtivacao');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('codigoDeAtivacao',codigoDeAtivacao,tcStr);
  FParametro.GerarParametro('opcao',opcao,tcInt);
  FParametro.GerarParametro('novoCodigo',novoCodigo,tcStr);
  FParametro.GerarParametro('confNovoCodigo',novoCodigo,tcStr);
  FMetodo.FinalizarMetodo;

  Resp := FComandoMFE.EnviaComando(numeroSessao,'TrocarCodigoDeAtivacao',FGerador.ArquivoFormatoXML);

  Result := FComandoMFE.PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.EnviarPagamento(Pagamento: TEnviarPagamento
  ): TRespostaPagamento;
var
  Comando, SATResp : String;
begin
  Result := Nil;
  TACBrSAT(Owner).IniciaComando;
  try
    Pagamento.Identificador := numeroSessao;
    Comando := Pagamento.AsXMLString;
    TACBrSAT(Owner).DoLog('EnviarPagamento( '+Comando+' )');

    SATResp := FComandoMFE.EnviaComando(numeroSessao,'EnviarPagamento',Comando);

    Result := TRespostaPagamento.Create;
    Result.AsXMLString := SATResp;
  finally
    TACBrSAT(Owner).FinalizaComando( SATResp );
  end;
end;

function TACBrSATMFe_integrador_XML.VerificarStatusValidador(
  VerificarStatusValidador: TVerificarStatusValidador
  ): TRespostaVerificarStatusValidador;
var
  Comando, SATResp : String;
begin
  Result := Nil;
  TACBrSAT(Owner).IniciaComando;
  try
    VerificarStatusValidador.Identificador := numeroSessao;
    Comando := VerificarStatusValidador.AsXMLString;
    TACBrSAT(Owner).DoLog('VerificarStatusValidador( '+Comando+' )');

    SATResp := FComandoMFE.EnviaComando(numeroSessao,'VerificarStatusValidador',Comando);

    Result := TRespostaVerificarStatusValidador.Create;
    Result.AsXMLString := SATResp;
  finally
    TACBrSAT(Owner).FinalizaComando( SATResp );
  end;
end;

function TACBrSATMFe_integrador_XML.RespostaFiscal(
  RespostaFiscal: TRespostaFiscal): TRetornoRespostaFiscal;
var
  Comando, SATResp : String;
begin
  Result := Nil;
  TACBrSAT(Owner).IniciaComando;
  try
    RespostaFiscal.Identificador := numeroSessao;
    Comando := RespostaFiscal.AsXMLString;
    TACBrSAT(Owner).DoLog('RespostaFiscal( '+Comando+' )');

    SATResp := FComandoMFE.EnviaComando(numeroSessao,'RespostaFiscal',Comando);

    Result := TRetornoRespostaFiscal.Create;
    Result.AsXMLString := SATResp;
  finally
    TACBrSAT(Owner).FinalizaComando( SATResp );
  end;
end;

function TACBrSATMFe_integrador_XML.EnviarStatusPagamento(
  StatusPagamento: TStatusPagamento): TRespostaStatusPagamento;
var
  Comando, Resp : String;
begin
  Result := Nil;
  TACBrSAT(Owner).IniciaComando;
  try
    StatusPagamento.Identificador := numeroSessao;
    Comando := StatusPagamento.AsXMLString;
    TACBrSAT(Owner).DoLog('EnviarStatusPagamento( '+Comando+' )');

    Resp := FComandoMFE.EnviaComando(numeroSessao,'EnviarStatusPagamento',Comando);

    Result := TRespostaStatusPagamento.Create;
    Result.AsXMLString := Resp;
  finally
    TACBrSAT(Owner).FinalizaComando( Resp );
  end;
end;

end.

