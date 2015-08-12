{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2014   Juliomar Marchetti                   }
{					  Isaque Pinheiro		       }
{ 					  Daniel Simões de Almeida	       }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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

{******************************************************************************
|* Historico
|*
*******************************************************************************}

{$I ACBr.inc}

unit ACBrECFBloco_J;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrECFBlocos;

type
  /// Registro J001 - Abertura do Bloco J – Plano de Contas e Mapeamento
  TRegistroJ001 = class(TOpenBlocos)
  end;

  /// Registro J050 - Plano de Contas do Contribuinte

  { TRegistroJ050 }

  TRegistroJ050 = class(TBlocos)
  private
    fCOD_CTA_SUP: string;
    fCTA:     string;
    fNIVEL:   integer;
    fIND_CTA: string;
    fCOD_NAT: string;
    fDT_ALT:  TDate;
    fCOD_CTA: string;
  public
    property DT_ALT: TDate read fDT_ALT write fDT_ALT;
    property COD_NAT: string read fCOD_NAT write fCOD_NAT;
    property IND_CTA: string read fIND_CTA write fIND_CTA;
    property NIVEL: integer read fNIVEL write fNIVEL;
    property COD_CTA: string read fCOD_CTA write fCOD_CTA;
    property COD_CTA_SUP: string read fCOD_CTA_SUP write fCOD_CTA_SUP;
    property CTA: string read fCTA write fCTA;
  end;

  /// Registro J051 - Plano de Contas Referencial
  TRegistroJ051 = class(TBlocos)
  private
    fCOD_CCUS:    string;
    fCOD_CTA_REF: string;
  public
    property COD_CCUS: string read fCOD_CCUS write fCOD_CCUS;
    property COD_CTA_REF: string read fCOD_CTA_REF write fCOD_CTA_REF;
  end;

  /// Registro J053 - Subcontas Correlatas
  TRegistroJ053 = class(TBlocos)
  private
    fCOD_CNT_CORR: string;
    fNAT_SUB_CNT:  string;
    fCOD_IDT:      string;
  public
    property COD_IDT: string read fCOD_IDT write fCOD_IDT;
    property COD_CNT_CORR: string read fCOD_CNT_CORR write fCOD_CNT_CORR;
    property NAT_SUB_CNT: string read fNAT_SUB_CNT write fNAT_SUB_CNT;
  end;

  /// Registro J100 - Centro de Custos
  TRegistroJ100 = class(TBlocos)
  private
    fDT_ALT:   TDate;
    fCOD_CCUS: string;
    fCCUS:     string;
  public
    property DT_ALT: TDate read fDT_ALT write fDT_ALT;
    property COD_CCUS: string read fCOD_CCUS write fCOD_CCUS;
    property CCUS: string read fCCUS write fCCUS;
  end;

  /// Registro J990 - ENCERRAMENTO DO BLOCO J
  TRegistroJ990 = class(TCloseBlocos)
  end;


implementation

end.
