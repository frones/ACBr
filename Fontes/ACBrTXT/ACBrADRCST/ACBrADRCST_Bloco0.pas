{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Ribamar M. Santos                               }
{                              Juliomar Marchetti                              }
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

unit ACBrADRCST_Bloco0;

interface

Uses
  SysUtils,
  Classes,
  ACBrADRCST_Blocos,
  ACBrADRCSTConversao;

type

  { TRegistro0000 }

  TRegistro0000 = class(TBlocos)
  private
    FCOD_VERSAO: TADRCSTVersao;
    FMES_ANO: TDateTime;
    FCNPJ: string;
    FIE: string;
    FNOME: string;
    FCD_FIN: TADRCSTFinalidade;
    FN_REG_ESPECIAL: string;
    FCNPJ_CD: string;
    FIE_CD: string;
    FOPCAO_R1200: TADRCSTIndicadorReaverRecolherImposto;
    FOPCAO_R1300: TADRCSTIndicadorReaverRecolherImposto;
    FOPCAO_R1400: TADRCSTIndicadorReaverRecolherImposto;
    FOPCAO_R1500: TADRCSTIndicadorReaverRecolherImposto;

  public
    constructor Create;overload;

    property COD_VERSAO: TADRCSTVersao read FCOD_VERSAO write FCOD_VERSAO;
    property MES_ANO: TDateTime read FMES_ANO write FMES_ANO;
    property CNPJ: string read FCNPJ write FCNPJ;
    property IE: string read FIE write FIE;
    property NOME: string read FNOME write FNOME;
    property CD_FIN: TADRCSTFinalidade read FCD_FIN write FCD_FIN;
    property N_REG_ESPECIAL: string read FN_REG_ESPECIAL write FN_REG_ESPECIAL;
    property CNPJ_CD: string read FCNPJ_CD write FCNPJ_CD;
    property IE_CD: string read FIE_CD write FIE_CD;
    //mudar abaixo para enumeradores
    property OPCAO_R1200 : TADRCSTIndicadorReaverRecolherImposto read FOPCAO_R1200 write FOPCAO_R1200;
    property OPCAO_R1300 : TADRCSTIndicadorReaverRecolherImposto read FOPCAO_R1300 write FOPCAO_R1300;
    property OPCAO_R1400 : TADRCSTIndicadorReaverRecolherImposto read FOPCAO_R1400 write FOPCAO_R1400;
    property OPCAO_R1500 : TADRCSTIndicadorReaverRecolherImposto read FOPCAO_R1500 write FOPCAO_R1500;

  end;


  { TRegistro0001 }

  TRegistro0001 = class(TBlocos)
  private
    FCD_FIN: TADRCSTFinalidade;
    FCNPJ: string;
    FCOD_VERSAO: TADRCSTCDVersao;
    FIE: string;
    FMES_ANO: TDateTime;
    FNOME: string;
  public
    constructor Create;overload;

    property COD_VERSAO : TADRCSTCDVersao read FCOD_VERSAO write FCOD_VERSAO;
    property MES_ANO : TDateTime read FMES_ANO write FMES_ANO;
    property CNPJ : string read FCNPJ write FCNPJ;
    property IE : string read FIE write FIE;
    property NOME : string read FNOME write FNOME;
    property CD_FIN : TADRCSTFinalidade read FCD_FIN write FCD_FIN;
  end;



implementation

{ TRegistro0001 }

constructor TRegistro0001.Create;
begin
  inherited Create;
  FCOD_VERSAO := High(TADRCSTCDVersao);
  FCD_FIN := afnArquivoOriginal;
end;

{ TRegistro0000 }

constructor TRegistro0000.Create;
begin
  inherited Create;
  FCOD_VERSAO := High(TADRCSTVersao);//ultima versão sempre inicia
  FCD_FIN := afnArquivoOriginal;
end;

end.
