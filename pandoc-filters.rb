# Author: D F A Ligertwood <dfaligertwood@dunelm.org.uk>
# License: BSD3
#
# Port of pandocfilters.py by John MacFarlane, available at
# github.com/jgm/pandocfilters
#
# Functions to aid writing Ruby scripts that process the pandoc
# AST serialized as JSON.

require 'json'

module Pandoc
    def walk(&action)
        pandoc_map { |item| item.walk &action }
    end
    def owalk(&action)
        pandoc_map { |item| item.owalk &action }
    end
end

class Object
    def pandoc_map(*a) self end
    include PandocMethods
end

class Hash
    def pandoc_map(&block)
        map{ |k,v| [k, block.(v)] }.to_h
    end
    include PandocMethods

    def to_pandoc(meta)
        if self['t']
            PandocItem.new(self['t'], self['c'].to_pandoc(meta), meta)
        else
            super
        end
    end
end

class Array
    def pandoc_map(&block)
        a = Array.new
        each { |item|
            result = block.(item)
            if result.is_a? Array and not item.is_a? Array
                a += result
            else
                a << result
            end
        }
        a
    end
    include PandocMethods
end

class PandocItem < Hash
    attr_reader :format, :meta
    def type; self['t']; end
    def type=(a); self['t']=a; end
    def contents; self['c']; end
    def contents=(a); self['c']=a; end

    def initialize type, contents, meta, *a
        @format = ARGV[1] || String.new
        @meta, self.type, self.contents = meta, type, contents
        super *a
    end

    def walk(&action)
        result = action.call self.type, self.contents, self.format, self.meta
        result.walk &action or PandocItem.new(self.type, self.contents.walk(&action), self.meta)
    end

    def owalk(&action)
        result = action.call self
        result.owalk &action or self.contents.owalk &action
    end

    def attributes
        if self.contents[0].is_a? Array
            return {id: self.contents[0][0],
                    classes: self.contents[0][1]}.merge(self.contents[0][2].to_h)
        else
            return false
        end
    end

    def attributes=(attrs)
        begin
            attrs.update(self.attributes) { |k,o,n| o }
            keys = attrs.reject{ |k| k == "id" or k == "classes" }.to_a
            self.contents[0] = [attrs['id'], attrs['classes'], keys]
        rescue
            if attrs.is_a? Hash
                $stderr.puts "#{self.type} does not accept attributes."
            else
                $stderr.puts "Incorrect format for attributes."
            end
        end
    end

    # Stringify function. Example of how to use 'walk'. Note 'nil'
    # at end of function. This is necessary because in ruby nearly
    # everything returns a value, and we don't want to affect the
    # AST.

    def stringify
        result = String.new
        walk { |type, contents|
            case type
            when "Str"
                result << contents.to_s
            when "Code", "Math"
                result << contents[1].to_s
            when "LineBreak", "Space"
                result << " "
            end
            nil
        }
        result
    end

    # Constructors.

    class <<self
        block_elements = {
            HorizontalRule: 0,
            Null: 0,
            BlockQuote: 1,
            BulletList: 1,
            DefinitionList: 1,
            Para: 1,
            Plain: 1,
            CodeBlock: 2,
            Div: 2,
            OrderedList: 2,
            RawBlock: 2,
            Header: 3,
            Table: 5
        }

        inline_elements = {
            LineBreak: 0,
            Space: 0,
            Emph: 1,
            Note: 1,
            SmallCaps: 1,
            Str: 1,
            Strong: 1,
            Strikeout: 1,
            Subscript: 1,
            Superscript: 1,
            Cite: 2,
            Code: 2,
            Image: 2,
            Link: 2,
            Math: 2,
            Quoted: 2,
            RawInline: 2,
            Span: 2
        }

        inline_elements.merge(block_elements).each { |eltType, numargs|
            define_method(eltType) { |*args|
                raise ArgumentError, "#{eltType} expects #{numargs} arguments, \
                      but received #{args.length} arguments" unless numargs == args.length

                if numargs == 1
                    return PandocItem.new(eltType.to_s, args[0], {})
                else
                    return PandocItem.new(eltType.to_s, args, {})
                end
            }
        }
    end
end

class Pandoc < PandocItem
    def initialize document, *a
        meta = document[0]["unMeta"]
        contents = document[1].to_pandoc(meta)
        super "Document", contents, meta, *a
    end
    def to_json
        return [{"unMeta" => self.meta}, self.contents].to_json; 
    end
    def pandoc_map(&block)
        Pandoc.new([{"unmeta" => self.meta}, block.(self.contents)])
    end
    def walk(&action)
        pandoc_map { |item| item.walk &action }
    end
    def owalk(&action)
        pandoc_map { |item| item.owalk &action }
    end

    class <<self
        def from_stdin; new(JSON.load(STDIN)); end
        def toJSONFilter(&action)
            document = Pandoc.new(JSON.load(STDIN))
            puts document.class
            document = document.walk &action
            puts document.class
            $stdout.print document.to_json
        end
        def filter(&action)
            document = from_stdin
            document = document.owalk &action
            $stdout.print document.to_json
        end
    end
end
