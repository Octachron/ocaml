module A: sig
  module B: sig
    module C: sig
      module D: sig
        module E: sig
          module F: sig type x end -> sig type y end
          -> sig type z end -> sig type w end -> sig end
        end
      end
    end
  end
end = struct
  module B = struct
    module C = struct
      module D = struct
        module E = struct
          module F(X:sig type x end)(Y:sig type y' end)
            (W:sig type w end) = struct end
        end
      end
    end
  end
end
