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

unit ACBrADRCST_Bloco9;

interface

Uses
  SysUtils,
  Classes,
  ACBrADRCST_Blocos;

type
  TRegistro9000 = class(TBlocos)
  private
    FREG1200_ICMSST_RECUPERAR_RESSARCIR: currency;
    FREG1200_ICMSST_COMPLEMENTAR: currency;
    FREG1300_ICMSST_RECUPERAR_RESSARCIR: currency;
    FREG1400_ICMSST_RECUPERAR_RESSARCIR: currency;
    FREG1500_ICMSST_RECUPERAR_RESSARCIR: currency;
    FREG9000_FECOP_RESSARCIR: currency;
    FREG9000_FECOP_COMPLEMENTAR: currency;
  public
    //constructor Create;
    //destructor Destroy; override;

    property REG1200_ICMSST_RECUPERAR_RESSARCIR: currency read FREG1200_ICMSST_RECUPERAR_RESSARCIR write FREG1200_ICMSST_RECUPERAR_RESSARCIR;
    property REG1200_ICMSST_COMPLEMENTAR: currency read FREG1200_ICMSST_COMPLEMENTAR write FREG1200_ICMSST_COMPLEMENTAR;
    property REG1300_ICMSST_RECUPERAR_RESSARCIR: currency read FREG1300_ICMSST_RECUPERAR_RESSARCIR write FREG1300_ICMSST_RECUPERAR_RESSARCIR;
    property REG1400_ICMSST_RECUPERAR_RESSARCIR: currency read FREG1400_ICMSST_RECUPERAR_RESSARCIR write FREG1400_ICMSST_RECUPERAR_RESSARCIR;
    property REG1500_ICMSST_RECUPERAR_RESSARCIR: currency read FREG1500_ICMSST_RECUPERAR_RESSARCIR write FREG1500_ICMSST_RECUPERAR_RESSARCIR;
    property REG9000_FECOP_RESSARCIR: currency read FREG9000_FECOP_RESSARCIR write FREG9000_FECOP_RESSARCIR;
    property REG9000_FECOP_COMPLEMENTAR: currency  read FREG9000_FECOP_COMPLEMENTAR write FREG9000_FECOP_COMPLEMENTAR;
  end;

  TRegistro9999 = class(TCloseBlocos)
  public
  end;

implementation

end.
