{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{ Esse arquivo usa a classe  SynaSer   Copyright (c)2001-2003, Lukas Gebauer   }
{  Project : Ararat Synapse     (Found at URL: http://www.ararat.cz/synapse/)  }
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
|* 12/08/2010: Primeira Versao
|*    Daniel Simoes de Almeida e André Moraes
******************************************************************************}

unit ACBrIBGE ;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, contnrs,
  ACBrSocket ;

const
  CIBGE_URL = 'http://www.ibge.gov.br/home/geociencias/areaterritorial/area.php' ;

type

  EACBrIBGEException = class ( Exception );

  { TACBrIBGECidade }

  TACBrIBGECidade = class
    private
      fArea       : Double ;
      fCodMunicio : Integer ;
      fCodUF      : Integer ;
      fMunicipio  : String ;
      fUF         : String ;
    public
      constructor Create ;

      property Municipio  : String  read fMunicipio   write fMunicipio ;
      property CodMunicio : Integer read fCodMunicio  write fCodMunicio ;
      property UF         : String  read fUF          write fUF ;
      property CodUF      : Integer read fCodUF       write fCodUF ;
      property Area       : Double  read fArea        write fArea ;

  end ;

  { Lista de Objetos do tipo TACBrIBGECidade }

  { TACBrIBGECidades }

  TACBrIBGECidades = class(TObjectList)
    protected
      procedure SetObject (Index: Integer; Item: TACBrIBGECidade);
      function GetObject (Index: Integer): TACBrIBGECidade;
      procedure Insert (Index: Integer; Obj: TACBrIBGECidade);
    public
      function Add (Obj: TACBrIBGECidade): Integer;
      property Objects [Index: Integer]: TACBrIBGECidade
        read GetObject write SetObject; default;
    end;

	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}		
  TACBrIBGE = class( TACBrHTTP )
    private
      fCidades : TACBrIBGECidades ;
      fOnBuscaEfetuada : TNotifyEvent ;

      procedure ProcessaResposta ;
    public
      constructor Create(AOwner: TComponent); override;
      Destructor Destroy ; override ;

      property Cidades : TACBrIBGECidades  read fCidades ;

      function BuscarPorCodigo( const ACodigo : Integer ) : Integer ;
      function BuscarPorNome( const ACidade : String; const AUF: String = '';
        const Exata: Boolean = False; const ComparacaoCaseSensitive: Boolean = True ) : Integer ;

    published
      property OnBuscaEfetuada : TNotifyEvent read fOnBuscaEfetuada
         write fOnBuscaEfetuada ;
  end ;

implementation

uses ACBrUtil, strutils ;

{ TACBrIBGECidades }

procedure TACBrIBGECidades.SetObject(Index : Integer ; Item : TACBrIBGECidade) ;
begin
  inherited SetItem (Index, Item) ;
end ;

function TACBrIBGECidades.GetObject(Index : Integer) : TACBrIBGECidade ;
begin
  Result := inherited GetItem(Index) as TACBrIBGECidade ;
end ;

procedure TACBrIBGECidades.Insert(Index : Integer ; Obj : TACBrIBGECidade) ;
begin
  inherited Insert(Index, Obj);
end ;

function TACBrIBGECidades.Add(Obj : TACBrIBGECidade) : Integer ;
begin
  Result := inherited Add(Obj) ;
end ;


{ TACBrIBGECidade }

constructor TACBrIBGECidade.Create ;
begin
  inherited Create;

  fArea       := 0 ;
  fMunicipio  := '';
  fCodMunicio := 0 ;
  fUF         := '';
  fCodUF      := 0 ;
end ;

{ TACBrIBGE }

constructor TACBrIBGE.Create(AOwner : TComponent) ;
begin
  inherited Create(AOwner) ;

  fOnBuscaEfetuada := nil ;
  fCidades         := TACBrIBGECidades.create( True );
end ;

destructor TACBrIBGE.Destroy ;
begin
  fCidades.Free ;

  inherited Destroy ;
end ;

function TACBrIBGE.BuscarPorCodigo(const ACodigo : Integer) : Integer ;
begin
  fCidades.Clear;

  if ACodigo = 0 then
     raise EACBrIBGEException.Create( ACBrStr('Código do Município deve ser informado') );

  HTTPGet(CIBGE_URL + '?codigo='+IntToStr(ACodigo) ) ;
  ProcessaResposta;

  Result := fCidades.Count;

  if Assigned( OnBuscaEfetuada ) then
     OnBuscaEfetuada( Self );

end ;

function TACBrIBGE.BuscarPorNome(const ACidade : String ; const AUF : String ;
  const Exata : Boolean; const ComparacaoCaseSensitive: Boolean) : Integer ;
var
  I : Integer ;
  Param, CidadeAchar, CidadeLista: String ;
begin
  fCidades.Clear;

  Param := AjustaParam( ACidade ) ;
  if Param = '' then
     raise EACBrIBGEException.Create( ACBrStr('Nome do Município deve ser informado') );

  HTTPGet(CIBGE_URL + '?nome='+Param ) ;

  ProcessaResposta;

  // Aplicando filtros (se informados) //
  if (AUF <> '') or Exata then
  begin
    CidadeAchar := TiraAcentos(ACidade);
    if not ComparacaoCaseSensitive then
      CidadeAchar := UpperCase(CidadeAchar);

    I := 0;
    while I < fCidades.Count do
    begin
      if (AUF <> '') and (fCidades[I].UF <> AUF) then
      begin
         fCidades.Delete(I);
         continue;
      end;

      if Exata then
      begin
        CidadeLista := TiraAcentos(fCidades[I].Municipio);
        if not ComparacaoCaseSensitive then
          CidadeLista := UpperCase(CidadeLista);

        if (CidadeLista <> CidadeAchar) then
        begin
          fCidades.Delete(I);
          continue;
        end;
      end;

      Inc( I ) ;
    end ;
  end ;

  Result := fCidades.Count;

  if Assigned( OnBuscaEfetuada ) then
     OnBuscaEfetuada( Self );

end ;

procedure TACBrIBGE.ProcessaResposta ;
Var
  Buffer : String ;
  PI, PF, I, CodMun, CodUF : Integer ;
  SL : TStringList ;
  Cidade : TACBrIBGECidade ;
begin
  Cidades.Clear;

  Buffer := LowerCase(RespHTTP.Text) ;
  PI := pos('<div id="miolo_interno">', Buffer ) ;

  if PI > 0 then
  begin
    PI := PosEx('<table', Buffer, PI ) ;   // Inicio da Tabela
    if PI > 0 then
    begin
      PF := PosEx('</table>', Buffer, PI ) ;
      if PF > 0 then
      begin
        SL := TStringList.Create;
        try
          SL.Text := Trim( StripHTML( copy( RespHTTP.Text, PI, PF ) ));

          // DEBUG //
          // WriteToTXT( 'C:\TEMP\HTTP.txt', SL.Text );

          I := 0 ;
          while I < SL.Count do
          begin
            CodUF := StrToIntDef(Trim(SL[I]),0);

            if (CodUF > 0) and (CodUF < 100) then
            begin
              if (SL.Count-I) >= 5 then
              begin
                CodMun := StrToIntDef( Trim(SL[I+2]), 0);

                if CodMun > 0 then
                begin
                   Cidade := TACBrIBGECidade.Create;

                   Cidade.CodUF      := CodUF;
                   Cidade.UF         := Trim( SL[I+1] );
                   Cidade.CodMunicio := CodMun;
                   Cidade.Municipio  := Trim( SL[I+3] );
                   Cidade.Area       := StringToFloatDef( Trim( SL[I+4] ), 0 );

                   Cidades.Add(Cidade);
                   I := I + 4 ;
                end ;
              end ;
            end ;

            Inc(I) ;
          end ;
        finally
          SL.Free ;
        end ;
      end ;
    end ;
  end ;
end ;

end.

