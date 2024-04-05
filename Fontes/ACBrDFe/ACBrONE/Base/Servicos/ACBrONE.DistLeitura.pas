{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit ACBrONE.DistLeitura;

interface

uses
  SysUtils, Classes,
  ACBrDFeConsts,
  pcnConversao,
  ACBrONE.Conversao;

type

  { TDistLeitura }

  TDistLeitura = class
  private
    FVersao: string;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: string;
    FtpDist: TtpDist;
    FultNSU: string;
    FNSUFin: string;
    FCNPJOper: string;
    FcEQP: string;
    FcUF: Integer;
    FindCompRet: TIndicador;
    FindResumo: TIndicador;

  public
    function GerarXML: string;
    function ObterNomeArquivo: string;

    property versao: string          read Fversao     write Fversao;
    property tpAmb: TpcnTipoAmbiente read FtpAmb      write FtpAmb;
    property verAplic: string        read FverAplic   write FverAplic;
    property tpDist: TtpDist         read FtpDist     write FtpDist;
    property ultNSU: string          read FultNSU     write FultNSU;
    property NSUFin: string          read FNSUFin     write FNSUFin;
    property CNPJOper: string        read FCNPJOper   write FCNPJOper;
    property cEQP: string            read FcEQP       write FcEQP;
    property cUF: Integer            read FcUF        write FcUF;
    property indCompRet: TIndicador  read FindCompRet write FindCompRet;
    property indResumo: TIndicador   read FindResumo  write FindResumo;
  end;

implementation

uses
  ACBrUtil.Base,
  ACBrONE.Consts;

{ TDistLeitura }

function TDistLeitura.ObterNomeArquivo: string;
begin
  Result := FormatDateTime('yyyymmddhhnnss', Now) + '-ped-csc.xml';
end;

function TDistLeitura.GerarXML: string;
var
 sNSU, sNSUFin, stpDist: string;
begin
  sNSU := IntToStrZero(StrToInt64Def(ultNSU, 0), 15);

  if NSUFin <> '' then
    sNSUFin := IntToStrZero(StrToIntDef(NSUFin, 0), 15);

  case tpDist of
    tdEquipamento,
    tdOperador:
      begin
        if cEQP = '' then
          stpDist := '<CNPJOper>' + CNPJOper + '</CNPJOper>'
        else
        begin
          if tpDist = tdEquipamento then
            stpDist := '<cEQP>' + cEQP + '</cEQP>';
        end;
      end;

    tdUFMDFe,
    tdUFCaptura:
      stpDist := '<cUF>' + IntToStr(FcUF) + '</cUF>'
  else
    stpDist := '';
  end;

  if FindCompRet = tiSim  then
    stpDist := stpDist + '<indCompRet>' + '1' + '</indCompRet>';

  if FindResumo = tiSim  then
    stpDist := stpDist + '<indResumo>' + '1' + '</indResumo>';

  Result := '<oneDistLeitura ' + NAME_SPACE_ONE + ' versao="' + Versao + '">' +
              '<tpAmb>' + tpAmbToStr(tpAmb) + '</tpAmb>' +
              '<verAplic>' + verAplic + '</verAplic>' +
              '<tpDist>' + TpDistToStr(FtpDist) + '</tpDist>' +
              '<ultNSU>' + sNSU + '</ultNSU>' +
              '<NSUFin>' + sNSUFin + '</NSUFin>' +
                stpDist +
            '</oneDistLeitura>';
end;

end.

