{******************************************************************************}
{ Projeto: Componente ACBrNFSe                                                 }
{  Biblioteca multiplataforma de componentes Delphi                            }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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

unit ACBrProvedorInfisc;

interface

uses
  Classes, SysUtils,
  pnfsConversao, pcnAuxiliar,
  ACBrNFSeConfiguracoes, ACBrNFSeUtil, ACBrUtil, ACBrDFeUtil,
  {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, FileCtrl {$ENDIF};

type
  { TACBrProvedorInfisc }

 TProvedorInfisc = class(TProvedorClass)
  protected
   { protected }
  private
   { private }
  public
   { public }
   Constructor Create;

   function GetConfigCidade(ACodCidade, AAmbiente: Integer): TConfigCidade; OverRide;
   function GetConfigSchema(ACodCidade: Integer): TConfigSchema; OverRide;
   function GetConfigURL(ACodCidade: Integer): TConfigURL; OverRide;
   function GetURI(URI: String): String; OverRide;
   function GetAssinarXML(Acao: TnfseAcao): Boolean; OverRide;
   function GetValidarLote: Boolean; OverRide;

   function Gera_TagI(Acao: TnfseAcao; Prefixo3, Prefixo4, NameSpaceDad, Identificador, URI: String): AnsiString; OverRide;
   function Gera_CabMsg(Prefixo2, VersaoLayOut, VersaoDados, NameSpaceCab: String; ACodCidade: Integer): AnsiString; OverRide;
   function Gera_DadosSenha(CNPJ, Senha: String): AnsiString; OverRide;
   function Gera_TagF(Acao: TnfseAcao; Prefixo3: String): AnsiString; OverRide;

   function GeraEnvelopeRecepcionarLoteRPS(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; OverRide;
   function GeraEnvelopeConsultarSituacaoLoteRPS(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; OverRide;
   function GeraEnvelopeConsultarLoteRPS(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; OverRide;
   function GeraEnvelopeConsultarNFSeporRPS(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; OverRide;
   function GeraEnvelopeConsultarNFSe(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; OverRide;
   function GeraEnvelopeCancelarNFSe(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; OverRide;
   function GeraEnvelopeGerarNFSe(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; OverRide;
   function GeraEnvelopeRecepcionarSincrono(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; OverRide;
   function GeraEnvelopeConsultarSequencialRps(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; OverRide;

   function GetSoapAction(Acao: TnfseAcao; NomeCidade: String): String; OverRide;
   function GetRetornoWS(Acao: TnfseAcao; RetornoWS: AnsiString): AnsiString; OverRide;

   function GeraRetornoNFSe(Prefixo: String; RetNFSe: AnsiString; NomeCidade: String): AnsiString; OverRide;
   function GetLinkNFSe(ACodMunicipio, ANumeroNFSe: Integer; ACodVerificacao, AInscricaoM: String; AAmbiente: Integer): String; OverRide;
  end;

implementation

{ TProvedorInfisc }

constructor TProvedorInfisc.Create;
begin
 {----}
end;

function TProvedorInfisc.GetConfigCidade(ACodCidade,
  AAmbiente: Integer): TConfigCidade;
var
  ConfigCidade: TConfigCidade;
begin
  ConfigCidade.VersaoSoap    := '';
  ConfigCidade.Prefixo2      := '';
  ConfigCidade.Prefixo3      := '';
  ConfigCidade.Prefixo4      := '';
  ConfigCidade.Identificador := '';
  ConfigCidade.QuebradeLinha := ';';

  if AAmbiente = 1
   then case ACodCidade of
      4303905: ConfigCidade.NameSpaceEnvelope := ''; // Campo Bom/RS
      4307906: ConfigCidade.NameSpaceEnvelope := ''; // Farroupilha/RS
      4305108: ConfigCidade.NameSpaceEnvelope := ''; // Caxias do Sul/RS
      // Alterado Por Cleiver em 21/01/2015
      4314050: ConfigCidade.NameSpaceEnvelope := ''; // Parobe/RS
      4317400: ConfigCidade.NameSpaceEnvelope := ''; // Santiago/RS
   end
  else
    ConfigCidade.NameSpaceEnvelope := '';

  ConfigCidade.AssinaRPS   := False;
  ConfigCidade.AssinaLote  := (AAmbiente = 1);
  ConfigCidade.AssinaGerar := True;

  Result := ConfigCidade;
end;

function TProvedorInfisc.GetConfigSchema(ACodCidade: Integer): TConfigSchema;
var
 ConfigSchema: TConfigSchema;
begin
  ConfigSchema.VersaoCabecalho := '';
  ConfigSchema.VersaoDados     := '';
  ConfigSchema.VersaoXML       := '1';
  case ACodCidade of
    4303905: ConfigSchema.NameSpaceXML := ''; // Campo Bom/RS
    4307906: ConfigSchema.NameSpaceXML := ''; // Farroupilha/RS
    // Alterado Por Moro em 18/02/2015
    4305108: begin
                ConfigSchema.NameSpaceXML := '';    // Caxias do Sul/RS
                ConfigSchema.VersaoXML    := '1.1'; // Versão do XML para cidade tem que ser "1.1"
             end;
    // Alterado Por Cleiver em 21/01/2015
    4314050: ConfigSchema.NameSpaceXML := ''; // Parobe/RS
    4317400: ConfigSchema.NameSpaceXML := ''; // Santiago/RS
  end;
  ConfigSchema.Cabecalho        := '';
  ConfigSchema.ServicoEnviar    := 'nfse.xsd';
  ConfigSchema.ServicoConSit    := '';
  ConfigSchema.ServicoConLot    := ''; 
  ConfigSchema.ServicoConRps    := ''; 
  ConfigSchema.ServicoConNfse   := ''; 
  ConfigSchema.ServicoCancelar  := ''; 
  ConfigSchema.ServicoConSeqRps := ''; 
  ConfigSchema.DefTipos         := ''; 
  Result := ConfigSchema;
end;

function TProvedorInfisc.GetConfigURL(ACodCidade: Integer): TConfigURL;
var
 ConfigURL: TConfigURL;
begin
  ConfigURL.HomNomeCidade         := '';
  ConfigURL.HomRecepcaoLoteRPS    := '';
  ConfigURL.HomConsultaLoteRPS    := '';
  ConfigURL.HomConsultaNFSeRPS    := '';
  ConfigURL.HomConsultaSitLoteRPS := '';
  ConfigURL.HomConsultaNFSe       := '';
  ConfigURL.HomCancelaNFSe        := '';
  ConfigURL.ProNomeCidade         := '';

   case ACodCidade of
    4303905: // 15/01/2015 - Leandro do Couto
    begin // Campo Bom / RS
       ConfigURL.HomRecepcaoLoteRPS    := 'http://homol.campobom.infisc.com.br/portal/Servicos?wsdl';
       ConfigURL.HomConsultaLoteRPS    := 'http://homol.campobom.infisc.com.br/portal/Servicos?wsdl';
       ConfigURL.HomConsultaNFSeRPS    := 'http://homol.campobom.infisc.com.br/portal/Servicos?wsdl';
       ConfigURL.HomConsultaSitLoteRPS := 'http://homol.campobom.infisc.com.br/portal/Servicos?wsdl';
       ConfigURL.HomConsultaNFSe       := 'http://homol.campobom.infisc.com.br/portal/Servicos?wsdl';
       ConfigURL.HomCancelaNFSe        := 'http://homol.campobom.infisc.com.br/portal/Servicos?wsdl';
       ConfigURL.HomConsultaSeqRPS     := 'http://homol.campobom.infisc.com.br/portal/Servicos?wsdl';
       ConfigURL.ProRecepcaoLoteRPS    := 'https://nfse.campobom.rs.gov.br/portal/Servicos?wsdl';
       ConfigURL.ProConsultaLoteRPS    := 'https://nfse.campobom.rs.gov.br/portal/Servicos?wsdl';
       ConfigURL.ProConsultaNFSeRPS    := 'https://nfse.campobom.rs.gov.br/portal/Servicos?wsdl';
       ConfigURL.ProConsultaSitLoteRPS := 'https://nfse.campobom.rs.gov.br/portal/Servicos?wsdl';
       ConfigURL.ProConsultaNFSe       := 'https://nfse.campobom.rs.gov.br/portal/Servicos?wsdl';
       ConfigURL.ProCancelaNFSe        := 'https://nfse.campobom.rs.gov.br/portal/Servicos?wsdl';
       ConfigURL.ProConsultaSeqRPS     := 'https://nfse.campobom.rs.gov.br/portal/Servicos?wsdl';

    end;
    4307906:
     begin // Farroupilha/RS
       ConfigURL.HomRecepcaoLoteRPS    := 'http://homol.farroupilha.infisc.com.br/portal/Servicos?wsdl';
       ConfigURL.HomConsultaLoteRPS    := 'http://homol.farroupilha.infisc.com.br/portal/Servicos?wsdl';
       ConfigURL.HomConsultaNFSeRPS    := 'http://homol.farroupilha.infisc.com.br/portal/Servicos?wsdl';
       ConfigURL.HomConsultaSitLoteRPS := 'http://homol.farroupilha.infisc.com.br/portal/Servicos?wsdl';
       ConfigURL.HomConsultaNFSe       := 'http://homol.farroupilha.infisc.com.br/portal/Servicos?wsdl';
       ConfigURL.HomCancelaNFSe        := 'http://homol.farroupilha.infisc.com.br/portal/Servicos?wsdl';
       ConfigURL.HomConsultaSeqRPS     := 'http://homol.farroupilha.infisc.com.br/portal/Servicos?wsdl';
       ConfigURL.ProRecepcaoLoteRPS    := 'https://dmse.farroupilha.rs.gov.br/portal/Servicos?wsdl';
       ConfigURL.ProConsultaLoteRPS    := 'https://dmse.farroupilha.rs.gov.br/portal/Servicos?wsdl';
       ConfigURL.ProConsultaNFSeRPS    := 'https://dmse.farroupilha.rs.gov.br/portal/Servicos?wsdl';
       ConfigURL.ProConsultaSitLoteRPS := 'https://dmse.farroupilha.rs.gov.br/portal/Servicos?wsdl';
       ConfigURL.ProConsultaNFSe       := 'https://dmse.farroupilha.rs.gov.br/portal/Servicos?wsdl';
       ConfigURL.ProCancelaNFSe        := 'https://dmse.farroupilha.rs.gov.br/portal/Servicos?wsdl';
       ConfigURL.ProConsultaSeqRPS     := 'https://dmse.farroupilha.rs.gov.br/portal/Servicos?wsdl';
     end;
    4305108:
     begin // Caxias do Sul/RS
       ConfigURL.HomRecepcaoLoteRPS    := 'https://nfsehomol.caxias.rs.gov.br/portal/Servicos?wsdl';
       ConfigURL.HomConsultaLoteRPS    := 'https://nfsehomol.caxias.rs.gov.br/portal/Servicos?wsdl';
       ConfigURL.HomConsultaNFSeRPS    := 'https://nfsehomol.caxias.rs.gov.br/portal/Servicos?wsdl';
       ConfigURL.HomConsultaSitLoteRPS := 'https://nfsehomol.caxias.rs.gov.br/portal/Servicos?wsdl';
       ConfigURL.HomConsultaNFSe       := 'https://nfsehomol.caxias.rs.gov.br/portal/Servicos?wsdl';
       ConfigURL.HomCancelaNFSe        := 'https://nfsehomol.caxias.rs.gov.br/portal/Servicos?wsdl';
       ConfigURL.HomConsultaSeqRPS     := 'https://nfsehomol.caxias.rs.gov.br/portal/Servicos?wsdl';
       ConfigURL.ProRecepcaoLoteRPS    := 'https://nfse.caxias.rs.gov.br/portal/Servicos?wsdl';
       ConfigURL.ProConsultaLoteRPS    := 'https://nfse.caxias.rs.gov.br/portal/Servicos?wsdl';
       ConfigURL.ProConsultaNFSeRPS    := 'https://nfse.caxias.rs.gov.br/portal/Servicos?wsdl';
       ConfigURL.ProConsultaSitLoteRPS := 'https://nfse.caxias.rs.gov.br/portal/Servicos?wsdl';
       ConfigURL.ProConsultaNFSe       := 'https://nfse.caxias.rs.gov.br/portal/Servicos?wsdl';
       ConfigURL.ProCancelaNFSe        := 'https://nfse.caxias.rs.gov.br/portal/Servicos?wsdl';
       ConfigURL.ProConsultaSeqRPS     := 'https://nfse.caxias.rs.gov.br/portal/Servicos?wsdl';
     end;
    // Alterado Por Cleiver em 21/01/2015
    4314050:
     begin // Parobe/RS
       ConfigURL.HomRecepcaoLoteRPS    := 'http://homol.parobe.infisc.com.br/portal/Servicos?wsdl';
       ConfigURL.HomConsultaLoteRPS    := 'http://homol.parobe.infisc.com.br/portal/Servicos?wsdl';
       ConfigURL.HomConsultaNFSeRPS    := 'http://homol.parobe.infisc.com.br/portal/Servicos?wsdl';
       ConfigURL.HomConsultaSitLoteRPS := 'http://homol.parobe.infisc.com.br/portal/Servicos?wsdl';
       ConfigURL.HomConsultaNFSe       := 'http://homol.parobe.infisc.com.br/portal/Servicos?wsdl';
       ConfigURL.HomCancelaNFSe        := 'http://homol.parobe.infisc.com.br/portal/Servicos?wsdl';
       ConfigURL.HomConsultaSeqRPS     := 'http://homol.parobe.infisc.com.br/portal/Servicos?wsdl';
       ConfigURL.ProRecepcaoLoteRPS    := 'https://nfse.parobe.rs.gov.br/portal/Servicos?wsdl';
       ConfigURL.ProConsultaLoteRPS    := 'https://nfse.parobe.rs.gov.br/portal/Servicos?wsdl';
       ConfigURL.ProConsultaNFSeRPS    := 'https://nfse.parobe.rs.gov.br/portal/Servicos?wsdl';
       ConfigURL.ProConsultaSitLoteRPS := 'https://nfse.parobe.rs.gov.br/portal/Servicos?wsdl';
       ConfigURL.ProConsultaNFSe       := 'https://nfse.parobe.rs.gov.br/portal/Servicos?wsdl';
       ConfigURL.ProCancelaNFSe        := 'https://nfse.parobe.rs.gov.br/portal/Servicos?wsdl';
       ConfigURL.ProConsultaSeqRPS     := 'https://nfse.parobe.rs.gov.br/portal/Servicos?wsdl';
     end;
    4317400:
     begin // Santiago/RS
       ConfigURL.HomRecepcaoLoteRPS    := 'http://homol.santiago.infisc.com.br/portal/Servicos?wsdl';
       ConfigURL.HomConsultaLoteRPS    := 'http://homol.santiago.infisc.com.br/portal/Servicos?wsdl';
       ConfigURL.HomConsultaNFSeRPS    := 'http://homol.santiago.infisc.com.br/portal/Servicos?wsdl';
       ConfigURL.HomConsultaSitLoteRPS := 'http://homol.santiago.infisc.com.br/portal/Servicos?wsdl';
       ConfigURL.HomConsultaNFSe       := 'http://homol.santiago.infisc.com.br/portal/Servicos?wsdl';
       ConfigURL.HomCancelaNFSe        := 'http://homol.santiago.infisc.com.br/portal/Servicos?wsdl';
       ConfigURL.HomConsultaSeqRPS     := 'http://homol.santiago.infisc.com.br/portal/Servicos?wsdl';
       ConfigURL.ProRecepcaoLoteRPS    := 'https//dmse.pmsantiago.com.br/portal/Servicos?wsdl';
       ConfigURL.ProConsultaLoteRPS    := 'https//dmse.pmsantiago.com.br/portal/Servicos?wsdl';
       ConfigURL.ProConsultaNFSeRPS    := 'https//dmse.pmsantiago.com.br/portal/Servicos?wsdl';
       ConfigURL.ProConsultaSitLoteRPS := 'https//dmse.pmsantiago.com.br/portal/Servicos?wsdl';
       ConfigURL.ProConsultaNFSe       := 'https//dmse.pmsantiago.com.br/portal/Servicos?wsdl';
       ConfigURL.ProCancelaNFSe        := 'https//dmse.pmsantiago.com.br/portal/Servicos?wsdl';
       ConfigURL.ProConsultaSeqRPS     := 'https//dmse.pmsantiago.com.br/portal/Servicos?wsdl';
     end;

  end;
  Result := ConfigURL;
end;

function TProvedorInfisc.GetURI(URI: String): String;
begin
 Result := '';
end;

function TProvedorInfisc.GetAssinarXML(Acao: TnfseAcao): Boolean;
begin
 Result := False;
 case Acao of
   acRecepcionar: Result := False;
   acConsSit:     Result := True;
   acConsLote:    Result := False;
   acConsNFSeRps: Result := False;
   acConsNFSe:    Result := True;
   acCancelar:    Result := True;
   acGerar:       Result := False;
   acConsSecRps:  Result := False;
 end;
end;

function TProvedorInfisc.GetValidarLote: Boolean;
begin
 Result := False;
end;

function TProvedorInfisc.Gera_TagI(Acao: TnfseAcao; Prefixo3, Prefixo4,
  NameSpaceDad, Identificador, URI: String): AnsiString;
begin
 case Acao of
   acRecepcionar: Result := '';
   acConsSit:     Result := '<?xml version=''1.0'' encoding=''utf-8''?><pedidoStatusLote'+ NameSpaceDad;
   acConsLote:    Result := '';
   acConsNFSeRps: Result := '';
   acConsNFSe:    Result := '<?xml version=''1.0'' encoding=''utf-8''?><pedidoLoteNFSe'+ NameSpaceDad;
   // Alterado Por Moro em 18/02/2015
   // Trocado pedAnulaNFSe por pedCancelaNFSe
   acCancelar:    Result := '<?xml version=''1.0'' encoding=''utf-8''?><pedCancelaNFSe'+ NameSpaceDad;
   //acCancelar:    Result := '<?xml version=''1.0'' encoding=''utf-8''?><pedAnulaNFSe'+ NameSpaceDad;
   acGerar:       Result := '';
   acConsSecRps:  Result := '';
 end;
end;

function TProvedorInfisc.Gera_CabMsg(Prefixo2, VersaoLayOut, VersaoDados,
  NameSpaceCab: String; ACodCidade: Integer): AnsiString;
begin
 Result := '';
end;

function TProvedorInfisc.Gera_DadosSenha(CNPJ, Senha: String): AnsiString;
begin
 Result := '';
end;

function TProvedorInfisc.Gera_TagF(Acao: TnfseAcao; Prefixo3: String): AnsiString;
begin
 case Acao of
   acRecepcionar: Result := '';
   acConsSit:     Result := '</pedidoStatusLote>';
   acConsLote:    Result := '';
   acConsNFSeRps: Result := '';
   acConsNFSe:    Result := '</pedidoLoteNFSe>';
   // Alterado Por Moro em 18/02/2015
   // Trocado /pedAnulaNFSe por /pedCancelaNFSe
   acCancelar:    Result := '</pedCancelaNFSe>';
   //acCancelar:    Result := '</pedAnulaNFSe>';
   acGerar:       Result := '';
   acConsSecRps:  Result := '';
 end;
end;

function TProvedorInfisc.GeraEnvelopeRecepcionarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>'+
           '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"'+
           ' xmlns:xsd="http://www.w3.org/2001/XMLSchema"'+
           ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">'+
           '<soapenv:Body>'+
           '<ns1:enviarLoteNotas soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"'+
           ' xmlns:ns1="http://ws.pc.gif.com.br/">'+
           '<xml xsi:type="xsd:string">'+
           //<!-- Informacoes da nota fiscal segundo estrutura da tag NFS-e --! >
           StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
           //<!-- Informacoes da nota fiscal segundo estrutura da tag NFS-e --! >
           '</xml>'+
           '</ns1:enviarLoteNotas>'+
           '</soapenv:Body>'+
           '</soapenv:Envelope>';
end;

function TProvedorInfisc.GeraEnvelopeConsultarSituacaoLoteRPS(
  URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>'+
           '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"'+
           ' xmlns:xsd="http://www.w3.org/2001/XMLSchema"'+
           ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">'+
           '<soapenv:Body>'+
           '<ns1:obterCriticaLote soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"'+
           ' xmlns:ns1="http://ws.pc.gif.com.br/">'+
           '<xml xsi:type="xsd:string">'+
           //<!-- Informacoes da nota fiscal segundo estrutura da tag NFS-e --! >
           StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
           //<!-- Informacoes da nota fiscal segundo estrutura da tag NFS-e --! >
           '</xml>'+
           '</ns1:obterCriticaLote>'+
           '</soapenv:Body>'+
           '</soapenv:Envelope>';
end;

function TProvedorInfisc.GeraEnvelopeConsultarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '';
end;

function TProvedorInfisc.GeraEnvelopeConsultarNFSeporRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '';
end;

function TProvedorInfisc.GeraEnvelopeConsultarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin

 {
 result := '<?xml version="1.0" encoding="UTF-8"?>'+
           '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"'+
           ' xmlns:xsd="http://www.w3.org/2001/XMLSchema"'+
           ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">'+
           '<soapenv:Body>'+
           '<ns1:obterNotaFiscal soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"'+
           ' xmlns:ns1="http://ws.pc.gif.com.br/">'+
           '<xml xsi:type="xsd:string">'+
           //<!-- Informacoes da nota fiscal segundo estrutura da tag NFS-e --! >
           StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
           //<!-- Informacoes da nota fiscal segundo estrutura da tag NFS-e --! >
           '</xml>'+
           '</ns1:obterNotaFiscal>'+
           '</soapenv:Body>'+
           '</soapenv:Envelope>';

  }


 // Alterado por Leandro do Couto em 16/1/2015
// Trocado OberterNotaFiscal por ObterLoteNotaFiscal

 result := '<?xml version="1.0" encoding="UTF-8"?>'+
           '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"'+
           ' xmlns:xsd="http://www.w3.org/2001/XMLSchema"'+
           ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">'+
           '<soapenv:Body>'+
           '<ns1:obterLoteNotaFiscal soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"'+
           ' xmlns:ns1="http://ws.pc.gif.com.br/">'+
           '<xml xsi:type="xsd:string">'+
           //<!-- Informacoes da nota fiscal segundo estrutura da tag NFS-e --! >
           StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
           //<!-- Informacoes da nota fiscal segundo estrutura da tag NFS-e --! >
           '</xml>'+
           '</ns1:obterLoteNotaFiscal>'+
           '</soapenv:Body>'+
           '</soapenv:Envelope>';

end;

function TProvedorInfisc.GeraEnvelopeCancelarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>'+
           '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"'+
           ' xmlns:xsd="http://www.w3.org/2001/XMLSchema"'+
           ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">'+
           '<soapenv:Body>'+
           // Alterado Por Moro em 18/02/2015
           // Trocado anularNotaFiscal por cancelarNotaFiscal
           '<ns1:cancelarNotaFiscal soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"'+
           //'<ns1:anularNotaFiscal soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"'+
           ' xmlns:ns1="http://ws.pc.gif.com.br/">'+
           '<xml xsi:type="xsd:string">'+
           //<!-- Informacoes da nota fiscal segundo estrutura da tag NFS-e --! >
           StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
           //<!-- Informacoes da nota fiscal segundo estrutura da tag NFS-e --! >
           '</xml>'+
           '</ns1:cancelarNotaFiscal>'+
           //'</ns1:anularNotaFiscal>'+
           '</soapenv:Body>'+
           '</soapenv:Envelope>';
end;

function TProvedorInfisc.GeraEnvelopeGerarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '';
end;

function TProvedorInfisc.GeraEnvelopeRecepcionarSincrono(
  URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 Result := '';
end;

function TProvedorInfisc.GeraEnvelopeConsultarSequencialRps(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 Result := '';
end;

function TProvedorInfisc.GetSoapAction(Acao: TnfseAcao; NomeCidade: String): String;
begin
 case Acao of
   acRecepcionar: Result := '';
   acConsSit:     Result := '';
   acConsLote:    Result := '';
   acConsNFSeRps: Result := '';
   acConsNFSe:    Result := '';
   acCancelar:    Result := '';
   acGerar:       Result := '';
   acRecSincrono: Result := '';
   acConsSecRps:  Result := '';
 end;
end;

function TProvedorInfisc.GetRetornoWS(Acao: TnfseAcao; RetornoWS: AnsiString): AnsiString;
begin
 case Acao of
   acRecepcionar: Result := SeparaDados( RetornoWS, 'return' );
   acConsSit:     Result := SeparaDados( RetornoWS, 'return' );
   acConsLote:    Result := '';
   acConsNFSeRps: Result := '';
   acConsNFSe:    Result := SeparaDados( RetornoWS, 'return' );
   acCancelar:    Result := SeparaDados( RetornoWS, 'return' );
   acGerar:       Result := '';
   acConsSecRps:  Result := '';
 end;
end;

function TProvedorInfisc.GeraRetornoNFSe(Prefixo: String;
  RetNFSe: AnsiString; NomeCidade: String): AnsiString;
begin
  Result := '<?xml version=''1.0'' encoding=''utf-8''?>' +#13#10+
            '<NFS-e>' +#13#10+
             RetNFSe  +#13#10+
            '</NFS-e>';
end;

function TProvedorInfisc.GetLinkNFSe(ACodMunicipio, ANumeroNFSe: Integer;
  ACodVerificacao, AInscricaoM: String; AAmbiente: Integer): String;
begin
  Result := '';
  case ACodMunicipio of
    4303905 : REsult := 'https://nfse.campobom.rs.gov.br/portal/'; // 15/01/201 - Leandro do Couto
    4307906 : Result := 'https://dmse.farroupilha.rs.gov.br/portal/';
    4305108 : Result := 'https://nfse.caxias.rs.gov.br/portal/Servicos/';
    // Alterado Por Cleiver em 21/01/2015
    4314050 : Result := 'https://nfse.parobe.rs.gov.br/portal/Servicos/';
    4317400 : Result := 'https://dmse.pmsantiago.com.br/portal/Servicos/';
    end;
end;

end.

