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
  Classes, SysUtils, ACBrSATClass, pcnGerador, pcnMFeUtil, pcnLeitor,
  pcnMFePagamento;

type

   { TACBrSATMFe_integrador_XML }

   TACBrSATMFe_integrador_XML = class( TACBrSATClass )
   private
     FGerador: TGerador;
     FComandoMFE: TComandoMFe;
     FIdentificador: TIdentificador;
     FParametro: TParametro;
     FMetodo: TMetodo;

     FPastaInput : String;
     FPastaOutput : String;
     FTimeout : Integer;
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
     function RespostaFiscal( RespostaFiscal : TRespostaFiscal ): String;
     function EnviarStatusPagamento( StatusPagamento : TStatusPagamento ): String;

   published
     property PastaInput  : String  read FPastaInput  write FPastaInput;
     property PastaOutput : String  read FPastaOutput write FPastaOutput;
     property Timeout     : Integer read FTimeout     write FTimeout default 30;
   end;

implementation

Uses ACBrUtil, pcnConversao, dateutils, ACBrSAT;

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
  FComandoMFE    := TComandoMFe.Create;
  FIdentificador := TIdentificador.Create(FGerador);
  FParametro     := TParametro.Create(FGerador);
  FMetodo        := TMetodo.Create(FGerador);

  FPastaInput  := 'C:\Integrador\Input\';
  FPastaOutput := 'C:\Integrador\Output\';
  FTimeout     := 30;
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

  FMetodo.GerarMetodo(numeroSessao,'MF-e','AssociarAssinatura');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('codigoDeAtivacao',codigoDeAtivacao,tcStr);
  FParametro.GerarParametro('cnpjValue',CNPJvalue,tcStr);
  FParametro.GerarParametro('assinaturaCNPJs',assinaturaCNPJs,tcStr);
  FMetodo.FinalizarMetodo;

  Resp := FComandoMFE.EnviaComando(numeroSessao,FGerador.ArquivoFormatoXML);

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

  FMetodo.GerarMetodo(numeroSessao,'MF-e','AtivarMFe');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('subComando',subComando,tcInt);
  FParametro.GerarParametro('codigoDeAtivacao',codigoDeAtivacao,tcStr);
  FParametro.GerarParametro('CNPJ',CNPJ,tcStr);
  FParametro.GerarParametro('cUF',cUF,tcInt);
  FMetodo.FinalizarMetodo;

  Resp := FComandoMFE.EnviaComando(numeroSessao,FGerador.ArquivoFormatoXML);

  Result := FComandoMFE.PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.AtualizarSoftwareSAT : String ;
Var
  Resp : String;
begin
  FGerador.LayoutArquivoTXT.Clear;

  FGerador.ArquivoFormatoXML := '';
  FGerador.ArquivoFormatoTXT := '';

  FMetodo.GerarMetodo(numeroSessao,'MF-e','AtualizarSoftwareMFe');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('codigoDeAtivacao',codigoDeAtivacao,tcStr);
  FMetodo.FinalizarMetodo;

  Resp := FComandoMFE.EnviaComando(numeroSessao,FGerador.ArquivoFormatoXML);

  Result := FComandoMFE.PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.BloquearSAT : String ;
Var
  Resp : String;
begin
  FGerador.LayoutArquivoTXT.Clear;

  FGerador.ArquivoFormatoXML := '';
  FGerador.ArquivoFormatoTXT := '';

  FMetodo.GerarMetodo(numeroSessao,'MF-e','BloquearMFe');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('codigoDeAtivacao',codigoDeAtivacao,tcStr);
  FMetodo.FinalizarMetodo;

  Resp := FComandoMFE.EnviaComando(numeroSessao,FGerador.ArquivoFormatoXML);

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

  FMetodo.GerarMetodo(numeroSessao,'MF-e','CancelarUltimaVenda');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('codigoDeAtivacao',codigoDeAtivacao,tcStr);
  FParametro.GerarParametro('chave',chave,tcStr);
  FParametro.GerarParametro('dadosCancelamento','<![CDATA[' +FComandoMFE.AjustaComando(dadosCancelamento)+ ']]>',tcStr, False);
  FMetodo.FinalizarMetodo;

  Resp := FComandoMFE.EnviaComando(numeroSessao,FGerador.ArquivoFormatoXML);

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

  FMetodo.GerarMetodo(numeroSessao,'MF-e','ComunicarCertificadoICPBRASIL');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('codigoDeAtivacao',codigoDeAtivacao,tcStr);
  FParametro.GerarParametro('certificado',certificado,tcStr);
  FMetodo.FinalizarMetodo;

  Resp := FComandoMFE.EnviaComando(numeroSessao,FGerador.ArquivoFormatoXML);
  Result := FComandoMFE.PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.ConfigurarInterfaceDeRede(
  dadosConfiguracao : AnsiString) : String ;
Var
  Resp : String;
begin
{  Resp := xSAT_ConfigurarInterfaceDeRede( numeroSessao,
                 String(codigoDeAtivacao), String(dadosConfiguracao) ) ; }

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

  FMetodo.GerarMetodo(numeroSessao,'MF-e','ConsultarNumeroSessao');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('codigoDeAtivacao',codigoDeAtivacao,tcStr);
  FParametro.GerarParametro('cNumeroDeSessao',cNumeroDeSessao,tcInt);
  FMetodo.FinalizarMetodo;

  Resp := FComandoMFE.EnviaComando(numeroSessao,FGerador.ArquivoFormatoXML);

  Result := FComandoMFE.PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.ConsultarSAT : String ;
Var
  Resp : String;
begin
  FGerador.LayoutArquivoTXT.Clear;

  FGerador.ArquivoFormatoXML := '';
  FGerador.ArquivoFormatoTXT := '';

  FMetodo.GerarMetodo(numeroSessao,'MF-e','ConsultarMFe');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FMetodo.FinalizarMetodo;

  Resp := FComandoMFE.EnviaComando(numeroSessao,FGerador.ArquivoFormatoXML);

  Result := FComandoMFE.PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.ConsultarStatusOperacional : String ;
Var
  Resp : String;
begin
  FGerador.LayoutArquivoTXT.Clear;

  FGerador.ArquivoFormatoXML := '';
  FGerador.ArquivoFormatoTXT := '';

  FMetodo.GerarMetodo(numeroSessao,'MF-e','ConsultarStatusOperacional');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('codigoDeAtivacao',codigoDeAtivacao,tcStr);
  FMetodo.FinalizarMetodo;

  Resp := FComandoMFE.EnviaComando(numeroSessao,FGerador.ArquivoFormatoXML);

  Result := FComandoMFE.PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.DesbloquearSAT : String ;
Var
  Resp : String;
begin
  FGerador.LayoutArquivoTXT.Clear;

  FGerador.ArquivoFormatoXML := '';
  FGerador.ArquivoFormatoTXT := '';

  FMetodo.GerarMetodo(numeroSessao,'MF-e','DesbloquearMFe');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('codigoDeAtivacao',codigoDeAtivacao,tcStr);
  FMetodo.FinalizarMetodo;

  Resp := FComandoMFE.EnviaComando(numeroSessao,FGerador.ArquivoFormatoXML);

  Result := FComandoMFE.PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.EnviarDadosVenda(dadosVenda : AnsiString) : String ;
Var
  Resp : String;
begin
  FGerador.LayoutArquivoTXT.Clear;

  FGerador.ArquivoFormatoXML := '';
  FGerador.ArquivoFormatoTXT := '';

  FMetodo.GerarMetodo(numeroSessao,'MF-e','EnviarDadosVenda');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('codigoDeAtivacao',codigoDeAtivacao,tcStr);
  FParametro.GerarParametro('dadosVenda','<![CDATA[' +FComandoMFE.AjustaComando(dadosVenda)+ ']]>',tcStr, False);
  FParametro.GerarParametro('nrDocumento',numeroSessao,tcInt);
  FMetodo.FinalizarMetodo;

  Resp := FComandoMFE.EnviaComando(numeroSessao,FGerador.ArquivoFormatoXML);
  Result := FComandoMFE.PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.ExtrairLogs : String ;
Var
  Resp : String;
begin
  FGerador.LayoutArquivoTXT.Clear;

  FGerador.ArquivoFormatoXML := '';
  FGerador.ArquivoFormatoTXT := '';

  FMetodo.GerarMetodo(numeroSessao,'MF-e','ExtrairLogs');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('codigoDeAtivacao',codigoDeAtivacao,tcStr);
  FMetodo.FinalizarMetodo;

  Resp := FComandoMFE.EnviaComando(numeroSessao,FGerador.ArquivoFormatoXML);
  Result := FComandoMFE.PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.TesteFimAFim(dadosVenda : AnsiString) : String ;
Var
  Resp : String;
begin
  FGerador.LayoutArquivoTXT.Clear;

  FGerador.ArquivoFormatoXML := '';
  FGerador.ArquivoFormatoTXT := '';

  FMetodo.GerarMetodo(numeroSessao,'MF-e','TesteFimAFim');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('codigoDeAtivacao',codigoDeAtivacao,tcStr);
  FParametro.GerarParametro('dadosVenda','<![CDATA[' +FComandoMFE.AjustaComando(dadosVenda)+ ']]>',tcStr, False);
 // FParametro.GerarParametro('nrDocumento',numeroSessao,tcInt);
  FMetodo.FinalizarMetodo;

  Resp := FComandoMFE.EnviaComando(numeroSessao,FGerador.ArquivoFormatoXML);

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

  FMetodo.GerarMetodo(numeroSessao,'MF-e','TrocarCodigoDeAtivacao');
  FParametro.GerarParametro('numeroSessao',numeroSessao,tcInt);
  FParametro.GerarParametro('codigoDeAtivacao',codigoDeAtivacao,tcStr);
  FParametro.GerarParametro('opcao',opcao,tcInt);
  FParametro.GerarParametro('novoCodigo',novoCodigo,tcStr);
  FParametro.GerarParametro('confNovoCodigo',novoCodigo,tcStr);
  FMetodo.FinalizarMetodo;

  Resp := FComandoMFE.EnviaComando(numeroSessao,FGerador.ArquivoFormatoXML);

  Result := FComandoMFE.PegaResposta( Resp );
end ;

function TACBrSATMFe_integrador_XML.EnviarPagamento(Pagamento: TEnviarPagamento
  ): TRespostaPagamento;
var
  Comando, Resp : String;
  RespostaPagamento : TRespostaPagamento;
begin
  TACBrSAT(Owner).IniciaComando;

  Pagamento.Identificador := numeroSessao;
  Comando := Pagamento.AsXMLString;
  TACBrSAT(Owner).fsComandoLog := 'EnviarPagamento( '+Comando+' )';


  Resp := FComandoMFE.EnviaComando(numeroSessao,Comando);

  RespostaPagamento := TRespostaPagamento.Create;
  RespostaPagamento.AsXMLString := Resp;

  Result := RespostaPagamento;

  TACBrSAT(Owner).FinalizaComando( Resp );
end;

function TACBrSATMFe_integrador_XML.VerificarStatusValidador(
  VerificarStatusValidador: TVerificarStatusValidador
  ): TRespostaVerificarStatusValidador;
var
  Comando, Resp : String;
  RespostaVerificarStatusValidador : TRespostaVerificarStatusValidador;
begin
  TACBrSAT(Owner).IniciaComando;

  VerificarStatusValidador.Identificador := numeroSessao;
  Comando := VerificarStatusValidador.AsXMLString;
  TACBrSAT(Owner).fsComandoLog := 'VerificarStatusValidador( '+Comando+' )';


  Resp := FComandoMFE.EnviaComando(numeroSessao,Comando);

  RespostaVerificarStatusValidador := TRespostaVerificarStatusValidador.Create;
  RespostaVerificarStatusValidador.AsXMLString := Resp;

  Result := RespostaVerificarStatusValidador;

  TACBrSAT(Owner).FinalizaComando( Resp );
end;

function TACBrSATMFe_integrador_XML.RespostaFiscal(
  RespostaFiscal: TRespostaFiscal): String;
var
  Comando, Resp : String;
begin
  TACBrSAT(Owner).IniciaComando;

  RespostaFiscal.Identificador := numeroSessao;
  Comando := RespostaFiscal.AsXMLString;
  TACBrSAT(Owner).fsComandoLog := 'RespostaFiscal( '+Comando+' )';

  Resp := FComandoMFE.EnviaComando(numeroSessao,Comando);

  Result := Resp;

  TACBrSAT(Owner).FinalizaComando( Resp );
end;

function TACBrSATMFe_integrador_XML.EnviarStatusPagamento(
  StatusPagamento: TStatusPagamento): String;
var
  Comando, Resp : String;
begin
  TACBrSAT(Owner).IniciaComando;

  StatusPagamento.Identificador := numeroSessao;
  Comando := StatusPagamento.AsXMLString;
  TACBrSAT(Owner).fsComandoLog := 'EnviarStatusPagamento( '+Comando+' )';

  Resp := FComandoMFE.EnviaComando(numeroSessao,Comando);

  Result := Resp;

  TACBrSAT(Owner).FinalizaComando( Resp );
end;

end.

