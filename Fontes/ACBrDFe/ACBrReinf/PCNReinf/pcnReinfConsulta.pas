{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Leivio Ramos de Fontenele                       }
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

{$I ACBr.inc}

unit pcnReinfConsulta;

interface

uses
  SysUtils, Classes, pcnConversao, pcnConversaoReinf, pcnGerador;

type

  TReinfConsulta = class(TObject)
  private
    FGerador: TGerador;
    FSoapEnvelope: String;

    FTipoEvento: TTipoEvento;
    FtpInscContrib: String;
    FnrInscContrib: String;
    FtpInscEstab: String;
    FnrInscEstab: String;
    FperApur: String;
    FcnpjPrestador: String;
    FtpInscTomador: String;
    FnrInscTomador: String;
    FdtApur: TDateTime;
    FnrInscAdq: String;
    FtpInscProd : String;
    FcpfCnpjBenef: String;
    FcnpjFonte: String;
  public
    constructor Create;
    destructor Destroy; override;
    function GerarXML: Boolean;
    procedure DefinirParametros(var URL: String);

    property Gerador: TGerador       read FGerador       write FGerador;
    property SoapEnvelope: String    read FSoapEnvelope  write FSoapEnvelope;

    property TipoEvento: TTipoEvento read FTipoEvento    write FTipoEvento;
    property nrInscContrib: String   read FnrInscContrib write FnrInscContrib;
    property nrInscEstab: String     read FnrInscEstab   write FnrInscEstab;
    property perApur: String         read FperApur       write FperApur;
    property cnpjPrestador: String   read FcnpjPrestador write FcnpjPrestador;
    property nrInscTomador: String   read FnrInscTomador write FnrInscTomador;
    property dtApur: TDateTime       read FdtApur        write FdtApur;
    property nrInscAdq: String       read FnrInscAdq     write FnrInscAdq;
    property cpfCnpjBenef: String    read FcpfCnpjBenef  write FcpfCnpjBenef;
    property cnpjFonte: String       read FcnpjFonte     write FcnpjFonte;
  end;

implementation

{ TReinfConsulta }

constructor TReinfConsulta.Create;
begin
  FGerador := TGerador.Create;
end;

procedure TReinfConsulta.DefinirParametros(var URL: String);
begin
  URL := URL +
         '/' + StringReplace(TipoEventoToStr(TipoEvento),'-','',[rfReplaceAll]);

  if Length(nrInscContrib) in [8,14] then
  begin
    if Length(nrInscContrib) = 14 then
      nrInscAdq := nrInscContrib;
    nrInscContrib := Copy( nrInscContrib, 1, 8 );
    FtpInscContrib := '1';
  end
  else
    FtpInscContrib := '2';

  if Length(nrInscEstab) = 14 then
  begin
    FtpInscEstab := '1';
  end
  else
    FtpInscEstab := '4';

  if Length(nrInscTomador) = 14 then
  begin
    FtpInscTomador := '1';
  end
  else
    FtpInscTomador := '4';

  if Length(nrInscEstab) = 14 then
  begin
    FtpInscProd := '1';
  end
  else
    FtpInscProd := '2';

  case TipoEvento of
    teR1000,
    teR1050,
    teR1070:
      begin
        URL := URL +
               '/' + FtpInscContrib +
               '/' + nrInscContrib;
      end;

    teR2010:
      begin
        URL := URL +
               '/' + FtpInscContrib +
               '/' + nrInscContrib +
               '/' + perApur +
               '/' + FtpInscEstab +
               '/' + nrInscEstab +
               '/' + cnpjPrestador;
      end;

    teR2020:
      begin
        URL := URL +
               '/' + FtpInscContrib +
               '/' + nrInscContrib +
               '/' + perApur +
               '/' + nrInscEstab +
               '/' + FtpInscTomador +
               '/' + nrInscTomador;
      end;

    teR2030,
    teR2040,
    teR2050:
      begin
        URL := URL +
               '/' + FtpInscContrib +
               '/' + nrInscContrib +
               '/' + perApur +
               '/' + nrInscEstab;
      end;

    teR2055:
      begin
        URL := URL +
               '/' + FtpInscContrib +
               '/' + nrInscContrib +
               '/' + perApur +
               '/' + FtpInscContrib +
               '/' + nrInscAdq +
               '/' + FtpInscProd +
               '/' + nrInscEstab;
      end;

    teR2060:
      begin
        URL := URL +
               '/' + FtpInscContrib +
               '/' + nrInscContrib +
               '/' + perApur +
               '/' + FtpInscEstab +
               '/' + nrInscEstab;
      end;

    teR2098,
    teR2099:
      begin
        URL := URL +
               '/' + FtpInscContrib +
               '/' + nrInscContrib +
               '/' + perApur;
      end;

    teR3010:
      begin
        URL := URL +
               '/' + FtpInscContrib +
               '/' + nrInscContrib +
               '/' + perApur +
               '/' + nrInscEstab;
      end;

    teR4010,
    teR4020:
      begin
        if cpfCnpjBenef = '' then
        begin
          if TipoEvento = teR4010 then
            URL := URL + '/semCpfBeneficiario'
          else
            URL := URL + '/semCnpjBeneficiario';
        end;

        URL := URL +
               '/' + FtpInscContrib +
               '/' + nrInscContrib +
               '/' + perApur +
               '/' + FtpInscEstab +
               '/' + nrInscEstab;

        if cpfCnpjBenef <> '' then
          URL := URL + '/' + cpfCnpjBenef;
      end;

    teR4040:
      begin
        URL := URL +
               '/' + FtpInscContrib +
               '/' + nrInscContrib +
               '/' + perApur +
               '/' + FtpInscEstab +
               '/' + nrInscEstab;
      end;

    teR4080:
      begin
        URL := URL +
               '/' + FtpInscContrib +
               '/' + nrInscContrib +
               '/' + perApur +
               '/' + FtpInscEstab +
               '/' + nrInscEstab +
               '/' + cnpjFonte;
      end;

    teR4099:
      begin
        URL := URL +
               '/' + FtpInscContrib +
               '/' + nrInscContrib +
               '/' + perApur;
      end;
  end;
end;

destructor TReinfConsulta.Destroy;
begin
  FGerador.Free;

  inherited;
end;

function TReinfConsulta.GerarXML: Boolean;
var
  tpEvento: string;
begin
  Gerador.ArquivoFormatoXML := '';
  tpEvento := Copy(TipoEventoToStr(TipoEvento), 3, 4);

  if Length(nrInscContrib) in [8,14] then
  begin
    if Length(nrInscContrib) = 14 then
      nrInscAdq := nrInscContrib;
    nrInscContrib := Copy( nrInscContrib, 1, 8 );
    FtpInscContrib := '1';
  end
  else
    FtpInscContrib := '2';

  if Length(nrInscEstab) = 14 then
  begin
    FtpInscEstab := '1';
  end
  else
    FtpInscEstab := '4';

  if Length(nrInscTomador) = 14 then
  begin
    FtpInscTomador := '1';
  end
  else
    FtpInscTomador := '4';

  if Length(nrInscEstab) = 14 then
  begin
    FtpInscProd := '1';
  end
  else
    FtpInscProd := '2';

  Gerador.Prefixo := '';

  Gerador.wGrupo('consultar ' + SoapEnvelope);

  Gerador.Prefixo := 'v1:';

 // Os 3 campos abaixos atende os Eventos: R-1000, R-1070

  Gerador.wCampo(tcStr, 'C02', 'tipoEvento', 04, 04, 1, tpEvento, 'XXX');
  // tpInsc e nrInsc é do Contribuinte
  Gerador.wCampo(tcInt, 'C03', 'tpInsc    ', 01, 01, 1, FtpInscContrib, 'XXX');
  Gerador.wCampo(tcStr, 'C05', 'nrInsc    ', 11, 14, 1, nrInscContrib, 'XXX');

  case TipoEvento of
    teR2010:
      begin
        Gerador.wCampo(tcStr, 'C06', 'perApur      ', 07, 07, 1, perApur, 'XXX');
        // tpInscEstab e nrInscEstab é do Estabelecimento
        Gerador.wCampo(tcStr, 'C07', 'tpInscEstab  ', 01, 01, 1, FtpInscEstab, 'XXX');
        Gerador.wCampo(tcStr, 'C08', 'nrInscEstab  ', 11, 14, 1, nrInscEstab, 'XXX');
        // cnpjPrestador é do Prestador de Serviço
        Gerador.wCampo(tcStr, 'C09', 'cnpjPrestador', 11, 14, 1, cnpjPrestador, 'XXX');
      end;

    teR2020:
      begin
        Gerador.wCampo(tcStr, 'C06', 'perApur         ', 07, 07, 1, perApur, 'XXX');
        // nrInscEstabPrest é do Estabelecimento
        Gerador.wCampo(tcStr, 'C07', 'nrInscEstabPrest', 11, 14, 1, nrInscEstab, 'XXX');
        // tpInscTomador e nrInscTomador é do Tomador
        Gerador.wCampo(tcStr, 'C07', 'tpInscTomador   ', 01, 01, 1, FtpInscTomador, 'XXX');
        Gerador.wCampo(tcStr, 'C09', 'nrInscTomador   ', 11, 14, 1, nrInscTomador, 'XXX');
      end;

    teR2030,
    teR2040,
    teR2050:
      begin
        Gerador.wCampo(tcStr, 'C06', 'perApur    ', 07, 07, 1, perApur, 'XXX');
        // nrInscEstabPrest é do Estabelecimento
        Gerador.wCampo(tcStr, 'C07', 'nrInscEstab', 11, 14, 1, nrInscEstab, 'XXX');
      end;

    teR2055:
      begin
        Gerador.wCampo(tcStr, 'C06', 'perApur    ', 07, 07, 1, perApur, 'XXX');
        // estabelecimento adquirente
        Gerador.wCampo(tcInt, 'C07', 'tpInscAdq    ', 01, 01, 1, FtpInscContrib, 'XXX');
        Gerador.wCampo(tcStr, 'C08', 'nrInscAdq    ', 11, 14, 1, nrInscAdq, 'XXX');
        //tipo inscrição produtor
        Gerador.wCampo(tcStr, 'C09', 'tpInscProd   ', 01, 01, 1, FtpInscProd, 'XXX');
        Gerador.wCampo(tcStr, 'C10', 'nrInscProd', 11, 14, 1, nrInscEstab, 'XXX');
      end;

    teR2060:
      begin
        Gerador.wCampo(tcStr, 'C06', 'perApur      ', 07, 07, 1, perApur, 'XXX');
        // tpInscEstab e nrInscEstab é do Estabelecimento
        Gerador.wCampo(tcStr, 'C07', 'tpInscEstab  ', 01, 01, 1, FtpInscEstab, 'XXX');
        Gerador.wCampo(tcStr, 'C08', 'nrInscEstab  ', 11, 14, 1, nrInscEstab, 'XXX');
      end;

    teR2098,
    teR2099:
      begin
        Gerador.wCampo(tcStr, 'C06', 'perApur', 07, 07, 1, perApur, 'XXX');
      end;

    teR3010:
      begin
        Gerador.wCampo(tcDat, 'C06', 'dtApur     ', 10, 10, 1, dtApur, 'XXX');
        // nrInscEstab é do Estabelecimento
        Gerador.wCampo(tcStr, 'C08', 'nrInscEstab', 11, 14, 1, nrInscEstab, 'XXX');
      end;
  end;

  Gerador.Prefixo := '';

  Gerador.wGrupo('/consultar');

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.

