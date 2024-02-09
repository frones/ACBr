{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
{                              Jean Carlo Cantu                                }
{                              Tiago Ravache                                   }
{                              Guilherme Costa                                 }
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

{******************************************************************************
|* Historico
|*
|* 27/10/2015: Jean Carlo Cantu, Tiago Ravache
|*  - Doação do componente para o Projeto ACBr
|* 28/08/2017: Leivio Fontenele - leivio@yahoo.com.br
|*  - Implementação comunicação, envelope, status e retorno do componente com webservice.
******************************************************************************}

{$I ACBr.inc}

unit pcesConsultaIdentEvt;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcesConversaoeSocial, pcnGerador;

type

  TConsultaIdentEvt = class(TObject)
  private
    FGerador: TGerador;
    FSoapEnvelope: String;
    FtipoConsulta: tpConsulta;

    FtpInsc: String;
    FnrInsc: String;
    FTipoEvento: TTipoEvento;
    FperApur: String;
    FchEvt: String;
    FdtIni: TDateTime;
    FdtFim: TDateTime;
    FcpfTrab: String;

  public
    constructor Create;
    destructor Destroy; override;
    function GerarXML: Boolean;
    property Gerador: TGerador        read FGerador      write FGerador;
    property SoapEnvelope: String     read FSoapEnvelope write FSoapEnvelope;
    property tipoConsulta: tpConsulta read FtipoConsulta write FtipoConsulta;

    property tpInsc: String           read FtpInsc       write FtpInsc;
    property nrInsc: String           read FnrInsc       write FnrInsc;
    property TipoEvento: TTipoEvento  read FTipoEvento   write FTipoEvento;
    property perApur: String          read FperApur      write FperApur;
    property chEvt: String            read FchEvt        write FchEvt;
    property dtIni: TDateTime         read FdtIni        write FdtIni;
    property dtFim: TDateTime         read FdtFim        write FdtFim;
    property cpfTrab: String          read FcpfTrab      write FcpfTrab;
  end;

implementation

{ TConsultaIdentEvt }

constructor TConsultaIdentEvt.Create;
begin
  inherited Create;
  FGerador := TGerador.Create;
end;

destructor TConsultaIdentEvt.Destroy;
begin
  FGerador.Free;

  inherited;
end;

function TConsultaIdentEvt.GerarXML: Boolean;
begin
  Gerador.ArquivoFormatoXML := '';

  Gerador.wGrupo('eSocial xmlns="' + SoapEnvelope + '"');
  Gerador.wGrupo('consultaIdentificadoresEvts');

  Gerador.wGrupo('ideEmpregador');
  Gerador.wCampo(tcInt, '', 'tpInsc', 01, 01, 1, tpInsc, 'XXX');
  Gerador.wCampo(tcStr, '', 'nrInsc', 11, 14, 1, nrInsc, 'XXX');
  Gerador.wGrupo('/ideEmpregador');

  case tipoConsulta of
    tcEmpregador:
      begin
        Gerador.wGrupo('consultaEvtsEmpregador');
        Gerador.wCampo(tcStr, '', 'tpEvt  ', 06, 06, 1, TipoEventoToStr(TipoEvento), 'XXX');
        Gerador.wCampo(tcStr, '', 'perApur', 07, 07, 1, perApur, 'XXX');
        Gerador.wGrupo('/consultaEvtsEmpregador');
      end;
    tcTabela:
      begin
        Gerador.wGrupo('consultaEvtsTabela');
        Gerador.wCampo(tcStr, '', 'tpEvt', 06, 006, 1, TipoEventoToStr(TipoEvento), 'XXX');
        Gerador.wCampo(tcStr, '', 'chEvt', 01, 100, 0, chEvt, 'XXX');
        Gerador.wCampo(tcDatHor, '', 'dtIni', 19, 19, 0, dtIni, 'XXX');
        Gerador.wCampo(tcDatHor, '', 'dtFim', 19, 19, 0, dtFim, 'XXX');
        Gerador.wGrupo('/consultaEvtsTabela');
      end;
    tcTrabalhador:
      begin
        Gerador.wGrupo('consultaEvtsTrabalhador');
        Gerador.wCampo(tcStr, '', 'cpfTrab', 11, 11, 1, cpfTrab, 'XXX');
        Gerador.wCampo(tcDatHor, '', 'dtIni', 19, 19, 1, dtIni, 'XXX');
        Gerador.wCampo(tcDatHor, '', 'dtFim', 19, 19, 1, dtFim, 'XXX');
        Gerador.wGrupo('/consultaEvtsTrabalhador');
      end;
  end;

  Gerador.wGrupo('/consultaIdentificadoresEvts');
  Gerador.wGrupo('/eSocial');

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.

