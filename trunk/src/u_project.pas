unit u_project;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { TProject }

  TProject = class(TPersistent)
    private
      fFileName,
        fOutPutFileName: AnsiString;
    published
      property OutPutFileName: AnsiString read fOutPutFileName write fOutPutFileName;
    public
      property FileName: AnsiString read fFileName write fFileName;
      constructor Create;
      destructor Destroy; override;
  end;

implementation

{ TProject }

constructor TProject.Create;
begin
  OutPutFileName := '';
  FileName := '';
end;

destructor TProject.Destroy;
begin
  inherited Destroy;
end;

end.

